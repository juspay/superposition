#!/usr/bin/env python3
"""
Test runner script for Superposition MCP Server tests.
Provides convenient interface for running different test suites.
"""

import argparse
import os
import subprocess
import sys
from pathlib import Path
from typing import List, Optional


class TestRunner:
    """Test runner for MCP server test suite."""
    
    def __init__(self, test_dir: Path):
        self.test_dir = test_dir
        self.base_cmd = ['python', '-m', 'pytest']
    
    def run_all_tests(self, coverage: bool = True, verbose: bool = False) -> int:
        """Run all tests."""
        cmd = self.base_cmd.copy()
        
        if coverage:
            cmd.extend(['--cov=src', '--cov-report=html', '--cov-report=term'])
        
        if verbose:
            cmd.append('-v')
        
        return self._run_command(cmd)
    
    def run_unit_tests(self, tool_type: Optional[str] = None, verbose: bool = False) -> int:
        """Run unit tests, optionally filtered by tool type."""
        cmd = self.base_cmd + ['-m', 'unit']
        
        if tool_type:
            if tool_type == 'cac':
                cmd.extend(['-m', 'cac'])
            elif tool_type == 'experimentation':
                cmd.extend(['-m', 'experimentation'])
            else:
                print(f"Unknown tool type: {tool_type}")
                return 1
        
        if verbose:
            cmd.append('-v')
        
        return self._run_command(cmd)
    
    def run_integration_tests(self, verbose: bool = False) -> int:
        """Run integration tests."""
        cmd = self.base_cmd + ['-m', 'integration']
        
        if verbose:
            cmd.append('-v')
        
        return self._run_command(cmd)
    
    def run_error_tests(self, verbose: bool = False) -> int:
        """Run error handling tests."""
        cmd = self.base_cmd + ['-m', 'error_handling']
        
        if verbose:
            cmd.append('-v')
        
        return self._run_command(cmd)
    
    def run_performance_tests(self, verbose: bool = False) -> int:
        """Run performance tests."""
        cmd = self.base_cmd + ['-m', 'performance']
        
        if verbose:
            cmd.append('-v')
        
        return self._run_command(cmd)
    
    def run_fast_tests(self, verbose: bool = False) -> int:
        """Run fast tests only (exclude slow tests)."""
        cmd = self.base_cmd + ['-m', 'not slow']
        
        if verbose:
            cmd.append('-v')
        
        return self._run_command(cmd)
    
    def run_specific_test(self, test_path: str, verbose: bool = False) -> int:
        """Run a specific test file or test function."""
        cmd = self.base_cmd + [test_path]
        
        if verbose:
            cmd.append('-v')
        
        return self._run_command(cmd)
    
    def check_test_env(self) -> bool:
        """Check if test environment is properly configured."""
        print("Checking test environment...")
        
        # Check if pytest is available
        try:
            result = subprocess.run(['python', '-m', 'pytest', '--version'], 
                                  capture_output=True, text=True)
            if result.returncode != 0:
                print("❌ pytest not available")
                return False
            print(f"✅ {result.stdout.strip()}")
        except FileNotFoundError:
            print("❌ Python not found")
            return False
        
        # Check if test requirements are installed
        requirements_file = self.test_dir / 'requirements.txt'
        if requirements_file.exists():
            print("✅ Test requirements file found")
            
            # Try to import key testing modules
            test_modules = ['pytest_asyncio', 'pytest_mock', 'pytest_cov']
            missing_modules = []
            
            for module in test_modules:
                try:
                    __import__(module)
                    print(f"✅ {module} available")
                except ImportError:
                    missing_modules.append(module)
                    print(f"❌ {module} not available")
            
            if missing_modules:
                print(f"\nTo install missing modules, run:")
                print(f"pip install -r {requirements_file}")
                return False
        
        # Check if test files exist
        test_files = [
            'unit/test_cac_tools.py',
            'unit/test_experimentation_tools.py',
            'integration/test_mcp_server.py'
        ]
        
        for test_file in test_files:
            file_path = self.test_dir / test_file
            if file_path.exists():
                print(f"✅ {test_file} found")
            else:
                print(f"❌ {test_file} not found")
                return False
        
        print("\n✅ Test environment is ready!")
        return True
    
    def generate_coverage_report(self) -> int:
        """Generate detailed coverage report."""
        print("Generating coverage report...")
        
        # Run tests with coverage
        cmd = self.base_cmd + [
            '--cov=src',
            '--cov-report=html:htmlcov',
            '--cov-report=term-missing',
            '--cov-report=xml:coverage.xml'
        ]
        
        result = self._run_command(cmd)
        
        if result == 0:
            print("\n📊 Coverage report generated:")
            print(f"  HTML: {self.test_dir.parent / 'htmlcov' / 'index.html'}")
            print(f"  XML:  {self.test_dir.parent / 'coverage.xml'}")
        
        return result
    
    def clean_test_artifacts(self) -> None:
        """Clean up test artifacts and cache files."""
        print("Cleaning test artifacts...")
        
        artifacts = [
            '.pytest_cache',
            '__pycache__',
            '*.pyc',
            'htmlcov',
            'coverage.xml',
            '.coverage'
        ]
        
        for artifact in artifacts:
            if artifact.startswith('.') or artifact.endswith('.xml'):
                # Handle files and directories in project root
                path = self.test_dir.parent / artifact
                if path.exists():
                    if path.is_file():
                        path.unlink()
                        print(f"🗑️  Removed {path}")
                    elif path.is_dir():
                        import shutil
                        shutil.rmtree(path)
                        print(f"🗑️  Removed {path}/")
            else:
                # Handle glob patterns
                import glob
                for match in glob.glob(str(self.test_dir.parent / '**' / artifact), recursive=True):
                    path = Path(match)
                    if path.is_file():
                        path.unlink()
                        print(f"🗑️  Removed {path}")
        
        print("✅ Test artifacts cleaned")
    
    def _run_command(self, cmd: List[str]) -> int:
        """Run a command and return exit code."""
        print(f"Running: {' '.join(cmd)}")
        
        # Change to test directory
        original_cwd = os.getcwd()
        try:
            os.chdir(self.test_dir)
            result = subprocess.run(cmd)
            return result.returncode
        finally:
            os.chdir(original_cwd)


def main():
    """Main entry point for test runner."""
    parser = argparse.ArgumentParser(
        description='Test runner for Superposition MCP Server',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s --all                    # Run all tests
  %(prog)s --unit                   # Run unit tests only
  %(prog)s --unit --cac             # Run CAC unit tests only
  %(prog)s --integration            # Run integration tests only
  %(prog)s --fast                   # Run fast tests only
  %(prog)s --specific tests/unit/test_cac_tools.py::TestCACTools::test_get_config_tool_success
  %(prog)s --coverage               # Generate coverage report
  %(prog)s --check                  # Check test environment
  %(prog)s --clean                  # Clean test artifacts
        """
    )
    
    parser.add_argument('--all', action='store_true',
                       help='Run all tests')
    parser.add_argument('--unit', action='store_true',
                       help='Run unit tests')
    parser.add_argument('--integration', action='store_true',
                       help='Run integration tests')
    parser.add_argument('--error', action='store_true',
                       help='Run error handling tests')
    parser.add_argument('--performance', action='store_true',
                       help='Run performance tests')
    parser.add_argument('--fast', action='store_true',
                       help='Run fast tests only (exclude slow tests)')
    
    parser.add_argument('--cac', action='store_true',
                       help='Filter to CAC-related tests (use with --unit)')
    parser.add_argument('--experimentation', action='store_true',
                       help='Filter to experimentation-related tests (use with --unit)')
    
    parser.add_argument('--specific', type=str,
                       help='Run specific test file or test function')
    
    parser.add_argument('--coverage', action='store_true',
                       help='Generate detailed coverage report')
    parser.add_argument('--check', action='store_true',
                       help='Check test environment setup')
    parser.add_argument('--clean', action='store_true',
                       help='Clean test artifacts and cache files')
    
    parser.add_argument('-v', '--verbose', action='store_true',
                       help='Verbose output')
    
    args = parser.parse_args()
    
    # Determine test directory
    test_dir = Path(__file__).parent
    runner = TestRunner(test_dir)
    
    # Handle special commands
    if args.check:
        if runner.check_test_env():
            return 0
        else:
            return 1
    
    if args.clean:
        runner.clean_test_artifacts()
        return 0
    
    if args.coverage:
        return runner.generate_coverage_report()
    
    # Handle test execution
    if args.all:
        return runner.run_all_tests(coverage=True, verbose=args.verbose)
    
    if args.unit:
        tool_type = None
        if args.cac:
            tool_type = 'cac'
        elif args.experimentation:
            tool_type = 'experimentation'
        return runner.run_unit_tests(tool_type=tool_type, verbose=args.verbose)
    
    if args.integration:
        return runner.run_integration_tests(verbose=args.verbose)
    
    if args.error:
        return runner.run_error_tests(verbose=args.verbose)
    
    if args.performance:
        return runner.run_performance_tests(verbose=args.verbose)
    
    if args.fast:
        return runner.run_fast_tests(verbose=args.verbose)
    
    if args.specific:
        return runner.run_specific_test(args.specific, verbose=args.verbose)
    
    # Default: run all tests
    print("No specific test type specified. Running all tests...")
    return runner.run_all_tests(coverage=True, verbose=args.verbose)


if __name__ == '__main__':
    sys.exit(main())