#!/usr/bin/env python3

import json
import sys
import os
import subprocess
from typing import Any, Dict, List, Optional
import pandas as pd
from pathlib import Path


class MCPServer:
    def __init__(self):
        self.project_root = Path(__file__).parent
        self.data_dir = self.project_root / "data"
        self.scripts_dir = self.project_root / "scripts"
        
    def list_tools(self) -> List[Dict[str, Any]]:
        """Return available MCP tools"""
        return [
            {
                "name": "list_r_scripts",
                "description": "List all R scripts in the project",
                "inputSchema": {
                    "type": "object",
                    "properties": {},
                    "required": []
                }
            },
            {
                "name": "read_flux_data",
                "description": "Read and summarize flux data files",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "file_pattern": {
                            "type": "string",
                            "description": "Pattern to match data files (e.g., '*.csv', 'processed_*')"
                        }
                    },
                    "required": []
                }
            },
            {
                "name": "run_r_script",
                "description": "Execute an R script from the scripts directory",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "script_name": {
                            "type": "string",
                            "description": "Name of the R script to run (without .r extension)"
                        },
                        "args": {
                            "type": "array",
                            "items": {"type": "string"},
                            "description": "Arguments to pass to the R script",
                            "default": []
                        }
                    },
                    "required": ["script_name"]
                }
            },
            {
                "name": "analyze_data_structure",
                "description": "Analyze the structure of data files in the project",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "data_type": {
                            "type": "string",
                            "enum": ["csv", "json", "all"],
                            "description": "Type of data files to analyze",
                            "default": "all"
                        }
                    },
                    "required": []
                }
            }
        ]
    
    def list_r_scripts(self) -> Dict[str, Any]:
        """List all R scripts with their descriptions"""
        scripts = []
        if self.scripts_dir.exists():
            for script_file in self.scripts_dir.glob("*.r"):
                script_info = {
                    "name": script_file.stem,
                    "path": str(script_file),
                    "size": script_file.stat().st_size
                }
                
                # Try to read first few lines for description
                try:
                    with open(script_file, 'r') as f:
                        lines = f.readlines()[:10]
                        comments = [line.strip() for line in lines if line.strip().startswith('#')]
                        if comments:
                            script_info["description"] = comments[0][1:].strip()
                except Exception:
                    script_info["description"] = "No description available"
                
                scripts.append(script_info)
        
        return {"scripts": scripts, "total": len(scripts)}
    
    def read_flux_data(self, file_pattern: str = "*.csv") -> Dict[str, Any]:
        """Read and summarize flux data files"""
        data_files = []
        
        # Search in data directory and subdirectories
        for pattern in [file_pattern]:
            data_files.extend(list(self.data_dir.rglob(pattern)))
        
        summaries = []
        for data_file in data_files[:10]:  # Limit to first 10 files
            try:
                if data_file.suffix.lower() == '.csv':
                    df = pd.read_csv(data_file)
                    summary = {
                        "file": str(data_file.relative_to(self.project_root)),
                        "rows": len(df),
                        "columns": len(df.columns),
                        "column_names": list(df.columns),
                        "size_mb": round(data_file.stat().st_size / 1024 / 1024, 2)
                    }
                    summaries.append(summary)
            except Exception as e:
                summaries.append({
                    "file": str(data_file.relative_to(self.project_root)),
                    "error": str(e)
                })
        
        return {"data_files": summaries, "total_found": len(data_files)}
    
    def run_r_script(self, script_name: str, args: List[str] = None) -> Dict[str, Any]:
        """Execute an R script"""
        if args is None:
            args = []
            
        script_path = self.scripts_dir / f"{script_name}.r"
        if not script_path.exists():
            return {"error": f"Script {script_name}.r not found in scripts directory"}
        
        try:
            # Run R script
            cmd = ["Rscript", str(script_path)] + args
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                cwd=str(self.project_root),
                timeout=300  # 5 minute timeout
            )
            
            return {
                "script": script_name,
                "exit_code": result.returncode,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "success": result.returncode == 0
            }
        except subprocess.TimeoutExpired:
            return {"error": f"Script {script_name} timed out after 5 minutes"}
        except Exception as e:
            return {"error": str(e)}
    
    def analyze_data_structure(self, data_type: str = "all") -> Dict[str, Any]:
        """Analyze the structure of data files"""
        analysis = {
            "directories": [],
            "file_types": {},
            "total_files": 0
        }
        
        # Analyze directory structure
        for item in self.data_dir.rglob("*"):
            if item.is_dir():
                rel_path = str(item.relative_to(self.data_dir))
                if rel_path != ".":
                    analysis["directories"].append(rel_path)
            else:
                analysis["total_files"] += 1
                ext = item.suffix.lower()
                if ext not in analysis["file_types"]:
                    analysis["file_types"][ext] = 0
                analysis["file_types"][ext] += 1
        
        # Filter by data type if specified
        if data_type != "all":
            if data_type == "csv":
                target_ext = ".csv"
            elif data_type == "json":
                target_ext = ".json"
            else:
                target_ext = f".{data_type}"
            
            analysis["filtered_count"] = analysis["file_types"].get(target_ext, 0)
        
        return analysis
    
    def handle_request(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """Handle MCP request"""
        method = request.get("method")
        params = request.get("params", {})
        
        if method == "tools/list":
            return {"tools": self.list_tools()}
        elif method == "tools/call":
            tool_name = params.get("name")
            arguments = params.get("arguments", {})
            
            if tool_name == "list_r_scripts":
                return {"content": [{"type": "text", "text": json.dumps(self.list_r_scripts(), indent=2)}]}
            elif tool_name == "read_flux_data":
                result = self.read_flux_data(arguments.get("file_pattern", "*.csv"))
                return {"content": [{"type": "text", "text": json.dumps(result, indent=2)}]}
            elif tool_name == "run_r_script":
                result = self.run_r_script(
                    arguments.get("script_name"),
                    arguments.get("args", [])
                )
                return {"content": [{"type": "text", "text": json.dumps(result, indent=2)}]}
            elif tool_name == "analyze_data_structure":
                result = self.analyze_data_structure(arguments.get("data_type", "all"))
                return {"content": [{"type": "text", "text": json.dumps(result, indent=2)}]}
            else:
                return {"error": {"code": -32601, "message": f"Unknown tool: {tool_name}"}}
        else:
            return {"error": {"code": -32601, "message": f"Unknown method: {method}"}}


def main():
    server = MCPServer()
    
    for line in sys.stdin:
        try:
            request = json.loads(line.strip())
            response = server.handle_request(request)
            print(json.dumps(response))
            sys.stdout.flush()
        except Exception as e:
            error_response = {"error": {"code": -32603, "message": str(e)}}
            print(json.dumps(error_response))
            sys.stdout.flush()


if __name__ == "__main__":
    main()