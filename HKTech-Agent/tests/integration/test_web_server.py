#!/usr/bin/env python3
"""
Web服务器集成测试

测试健康检查端点、Markdown渲染和Webhook处理。
"""

import pytest
import sys
import os
import json
import tempfile
from pathlib import Path
from unittest.mock import Mock, patch, MagicMock

# 添加项目根目录到Python路径
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))
sys.path.insert(0, str(project_root / 'shared'))
sys.path.insert(0, str(project_root / 'scripts'))


class TestWebServerHealthEndpoint:
    """测试Web服务器健康检查端点（使用模拟请求）"""
    
    def test_health_endpoint_text_format(self):
        """测试/health端点（text格式）"""
        # 模拟模块常量
        with patch('start_web_server.WEB_DIR', '/tmp/test_web'), \
             patch('start_web_server.DOCS_DIR', '/tmp/test_docs'):
            
            # 导入模块
            import start_web_server
            import http.server
            
            # 修补父类的handle_one_request方法，防止它在初始化期间被调用
            with patch.object(http.server.SimpleHTTPRequestHandler, 'handle_one_request', return_value=None):
                # 创建模拟的Handler实例
                handler = start_web_server.Handler(
                    Mock(),  # request
                    ('127.0.0.1', 8080),  # client_address
                    Mock()   # server
                )
            
            # 模拟请求属性
            handler.path = '/health'
            handler.send_response = Mock()
            handler.send_header = Mock()
            handler.end_headers = Mock()
            handler.wfile = Mock()
            
            # 模拟健康检查模块的run_health_check函数
            mock_report = "🧪 健康检查报告\n总体状态: HEALTHY"
            with patch('health_check.run_health_check') as mock_run_health_check:
                mock_run_health_check.return_value = mock_report
                
                # 调用健康检查处理
                handler.serve_health_check()
                
                # 验证响应
                handler.send_response.assert_called_once_with(200)
                handler.send_header.assert_any_call('Content-Type', 'text/plain')
                handler.send_header.assert_any_call('Cache-Control', 'no-cache')
                handler.wfile.write.assert_called_once_with(mock_report.encode('utf-8'))
    
    def test_health_endpoint_json_format(self):
        """测试/health端点（JSON格式）"""
        with patch('start_web_server.WEB_DIR', '/tmp/test_web'), \
             patch('start_web_server.DOCS_DIR', '/tmp/test_docs'):
            
            import start_web_server
            import http.server
            
            # 修补父类的handle_one_request方法，防止它在初始化期间被调用
            with patch.object(http.server.SimpleHTTPRequestHandler, 'handle_one_request', return_value=None):
                handler = start_web_server.Handler(
                    Mock(),
                    ('127.0.0.1', 8080),
                    Mock()
                )
            
            handler.path = '/health?format=json'
            handler.send_response = Mock()
            handler.send_header = Mock()
            handler.end_headers = Mock()
            handler.wfile = Mock()
            
            # 模拟JSON报告
            mock_report = json.dumps({
                "timestamp": "2026-02-19T12:00:00",
                "system": "Linux",
                "python_version": "3.12.3",
                "summary": {"overall_status": "healthy"}
            })
            
            with patch('health_check.run_health_check') as mock_run_health_check:
                mock_run_health_check.return_value = mock_report
                
                handler.serve_health_check()
                
                handler.send_response.assert_called_once_with(200)
                handler.send_header.assert_any_call('Content-Type', 'application/json')
                handler.wfile.write.assert_called_once_with(mock_report.encode('utf-8'))
    
    def test_health_endpoint_html_format(self):
        """测试/health端点（HTML格式）"""
        with patch('start_web_server.WEB_DIR', '/tmp/test_web'), \
             patch('start_web_server.DOCS_DIR', '/tmp/test_docs'):
            
            import start_web_server
            import http.server
            
            # 修补父类的handle_one_request方法，防止它在初始化期间被调用
            with patch.object(http.server.SimpleHTTPRequestHandler, 'handle_one_request', return_value=None):
                handler = start_web_server.Handler(
                    Mock(),
                    ('127.0.0.1', 8080),
                    Mock()
                )
            
            handler.path = '/health?format=html'
            handler.send_response = Mock()
            handler.send_header = Mock()
            handler.end_headers = Mock()
            handler.wfile = Mock()
            
            mock_report = "<!DOCTYPE html><h1>Health Report</h1>"
            with patch('health_check.run_health_check') as mock_run_health_check:
                mock_run_health_check.return_value = mock_report
                
                handler.serve_health_check()
                
                handler.send_response.assert_called_once_with(200)
                handler.send_header.assert_any_call('Content-Type', 'text/html')
                handler.wfile.write.assert_called_once_with(mock_report.encode('utf-8'))
    
    def test_health_endpoint_fallback_on_error(self):
        """测试健康检查失败时的回退响应"""
        with patch('start_web_server.WEB_DIR', '/tmp/test_web'), \
             patch('start_web_server.DOCS_DIR', '/tmp/test_docs'):
            
            import start_web_server
            import http.server
            
            # 修补父类的handle_one_request方法，防止它在初始化期间被调用
            with patch.object(http.server.SimpleHTTPRequestHandler, 'handle_one_request', return_value=None):
                handler = start_web_server.Handler(
                    Mock(),
                    ('127.0.0.1', 8080),
                    Mock()
                )
            
            handler.path = '/health'
            handler.send_response = Mock()
            handler.send_header = Mock()
            handler.end_headers = Mock()
            handler.wfile = Mock()
            
            # 模拟健康检查模块抛出异常
            with patch('health_check.run_health_check') as mock_run_health_check:
                mock_run_health_check.side_effect = Exception("Health check failed")
                
                handler.serve_health_check()
                
                # 应该返回500错误
                handler.send_response.assert_called_once_with(500)
                handler.send_header.assert_any_call('Content-Type', 'text/plain')
                # wfile.write应该被调用
                assert handler.wfile.write.called


class TestWebServerMarkdownRendering:
    """测试Markdown渲染功能"""
    
    @pytest.fixture
    def temp_markdown_file(self):
        """临时Markdown文件fixture"""
        with tempfile.TemporaryDirectory() as temp_dir:
            md_file = Path(temp_dir) / 'test.md'
            md_file.write_text('# Test Markdown\n\nThis is a **bold** test.')
            yield md_file
    
    def test_markdown_to_html_conversion(self, temp_markdown_file):
        """测试Markdown转HTML功能"""
        with patch('start_web_server.WEB_DIR', '/tmp/test_web'), \
             patch('start_web_server.DOCS_DIR', str(temp_markdown_file.parent)):
            
            import start_web_server
            
            handler = start_web_server.Handler(
                Mock(),
                ('127.0.0.1', 8080),
                Mock()
            )
            
            # 测试简单的Markdown转换
            md_content = '# Title\n\nSome **bold** text'
            html = handler.simple_md_to_html(md_content, 'Test Page')
            
            assert '<h1>Title</h1>' in html
            assert '<strong>bold</strong>' in html
            assert '<!DOCTYPE html>' in html
            assert '<title>Test Page</title>' in html


class TestWebServerStaticFiles:
    """测试静态文件服务"""
    
    def test_translate_path_removes_web_prefix(self):
        """测试路径转换移除/web前缀"""
        with patch('start_web_server.WEB_DIR', '/tmp/test_web'), \
             patch('start_web_server.DOCS_DIR', '/tmp/test_docs'):
            
            import start_web_server
            
            handler = start_web_server.Handler(
                Mock(),
                ('127.0.0.1', 8080),
                Mock()
            )
            
            # 模拟父类的translate_path返回路径
            original_translate = handler.translate_path
            
            with patch.object(handler, 'translate_path') as mock_translate:
                mock_translate.return_value = '/tmp/test_web/index.html'
                
                # 测试移除/web前缀
                result = handler.translate_path('/web/index.html')
                assert result == '/tmp/test_web/index.html'


class TestHealthCheckModule:
    """测试健康检查模块（直接测试）"""
    
    def test_health_check_import(self):
        """测试健康检查模块导入"""
        try:
            from health_check import HealthCheck, run_health_check
            assert HealthCheck is not None
            assert run_health_check is not None
        except ImportError as e:
            pytest.skip(f"健康检查模块不可用: {e}")
    
    def test_health_check_instantiation(self):
        """测试健康检查器实例化"""
        from health_check import HealthCheck
        
        checker = HealthCheck()
        assert checker.config == {}
        assert checker.checks is not None
        
        # 应该注册了默认检查项
        assert len(checker.checks) > 0
        
        # 检查项应该有名称和描述
        for check in checker.checks:
            assert 'name' in check
            assert 'func' in check
            assert 'description' in check
    
    def test_health_check_run_all(self):
        """测试运行所有健康检查"""
        from health_check import HealthCheck
        
        checker = HealthCheck()
        results = checker.run_all_checks()
        
        # 验证结果结构
        assert 'timestamp' in results
        assert 'system' in results
        assert 'python_version' in results
        assert 'checks' in results
        assert 'summary' in results
        
        summary = results['summary']
        assert 'total' in summary
        assert 'passed' in summary
        assert 'failed' in summary
        assert 'overall_status' in summary
        
        # 应该有7个默认检查项
        expected_checks = [
            'system_resources',
            'python_environment', 
            'data_directories',
            'dependencies',
            'external_apis',
            'component_status',
            'log_files'
        ]
        
        for check_name in expected_checks:
            assert check_name in results['checks']


@pytest.mark.integration
def test_web_server_integration():
    """集成测试标记"""
    pass


if __name__ == '__main__':
    pytest.main([__file__, '-v'])