#!/usr/bin/env python3
"""
统一Web服务器 - 监控界面 + 文档中心 + Webhook
"""

import http.server
import socketserver
import json
import os
import re
import sys

# 添加项目路径以导入健康检查模块
script_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.join(script_dir, '..')
shared_dir = os.path.join(project_root, 'shared')
if shared_dir not in sys.path:
    sys.path.insert(0, shared_dir)

PORT = 8080
WEB_DIR = "/opt/hktech-agent/web"
DOCS_DIR = "/opt/hktech-agent"

class Handler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=WEB_DIR, **kwargs)
    
    def translate_path(self, path):
        """重写路径解析"""
        # 移除 /web 前缀，直接映射到 WEB_DIR
        if path.startswith('/web/'):
            path = path[4:]  # /web/docs -> /docs
        return super().translate_path(path)
    
    def do_GET(self):
        """处理GET请求"""
        # 健康检查端点
        if self.path in ['/health', '/healthz', '/status']:
            self.serve_health_check()
            return
        
        # 如果是markdown文件，渲染为HTML
        if self.path.endswith('.md'):
            self.serve_markdown()
        else:
            super().do_GET()
    
    def serve_markdown(self):
        """将Markdown渲染为HTML"""
        try:
            file_path = os.path.join(DOCS_DIR, self.path.lstrip('/'))
            
            if not os.path.exists(file_path):
                self.send_error(404, "File not found")
                return
            
            with open(file_path, 'r', encoding='utf-8') as f:
                md_content = f.read()
            
            html_content = self.simple_md_to_html(md_content, os.path.basename(file_path))
            
            self.send_response(200)
            self.send_header('Content-type', 'text/html; charset=utf-8')
            self.end_headers()
            self.wfile.write(html_content.encode('utf-8'))
            
        except Exception as e:
            print(f"Error: {e}")
            self.send_error(500, str(e))
    
    def simple_md_to_html(self, md_content, title):
        """简化版Markdown转HTML"""
        lines = md_content.split('\n')
        html_lines = []
        in_code = False
        
        for line in lines:
            # 代码块
            if line.startswith('```'):
                if not in_code:
                    html_lines.append('<pre><code>')
                    in_code = True
                else:
                    html_lines.append('</code></pre>')
                    in_code = False
                continue
            
            if in_code:
                html_lines.append(line)
                continue
            
            # 标题
            if line.startswith('# '):
                html_lines.append(f'<h1>{line[2:]}</h1>')
            elif line.startswith('## '):
                html_lines.append(f'<h2>{line[3:]}</h2>')
            elif line.startswith('### '):
                html_lines.append(f'<h3>{line[4:]}</h3>')
            # 列表
            elif line.startswith('- '):
                html_lines.append(f'<li>{line[2:]}</li>')
            # 空行
            elif line.strip() == '':
                html_lines.append('')
            # 普通段落
            else:
                # 处理行内格式
                line = re.sub(r'\*\*(.+?)\*\*', r'<strong>\1</strong>', line)
                line = re.sub(r'\*(.+?)\*', r'<em>\1</em>', line)
                html_lines.append(f'<p>{line}</p>')
        
        body = '\n'.join(html_lines)
        
        return f'''<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>{title}</title>
    <style>
        body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; max-width: 900px; margin: 40px auto; padding: 20px; line-height: 1.6; background: #f5f7fa; }}
        .container {{ background: white; padding: 40px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); }}
        h1 {{ color: #3370ff; border-bottom: 2px solid #3370ff; padding-bottom: 10px; }}
        h2 {{ color: #1f2329; margin-top: 30px; border-bottom: 1px solid #e5e6eb; padding-bottom: 8px; }}
        h3 {{ color: #646a73; }}
        pre {{ background: #1e1e1e; color: #d4d4d4; padding: 15px; border-radius: 6px; overflow-x: auto; }}
        code {{ background: #f2f3f5; padding: 2px 6px; border-radius: 3px; font-family: monospace; }}
        li {{ margin: 5px 0; }}
        a {{ color: #3370ff; }}
        .nav {{ background: #121829; color: white; padding: 15px; margin: -40px -40px 30px -40px; border-radius: 8px 8px 0 0; }}
        .nav a {{ color: #85a5ff; margin-right: 20px; text-decoration: none; }}
    </style>
</head>
<body>
    <div class="container">
        <div class="nav">
            <a href="/">🏠 监控</a>
            <a href="/web/docs/index.html">📚 文档</a>
        </div>
        {body}
    </div>
</body>
</html>'''
    
    def serve_health_check(self):
        """提供健康检查端点"""
        try:
            # 导入健康检查模块
            import health_check
            
            # 获取查询参数
            import urllib.parse
            query = urllib.parse.urlparse(self.path).query
            params = urllib.parse.parse_qs(query)
            
            format = params.get('format', ['text'])[0]
            if format not in ['text', 'json', 'html']:
                format = 'text'
            
            # 运行健康检查
            report = health_check.run_health_check(format)
            
            # 设置响应头
            if format == 'json':
                content_type = 'application/json'
            elif format == 'html':
                content_type = 'text/html'
            else:
                content_type = 'text/plain'
            
            self.send_response(200)
            self.send_header('Content-Type', content_type)
            self.send_header('Cache-Control', 'no-cache')
            self.end_headers()
            
            # 发送响应
            if isinstance(report, str):
                self.wfile.write(report.encode('utf-8'))
            else:
                self.wfile.write(report)
                
        except Exception as e:
            # 健康检查失败时的回退响应
            import traceback
            error_msg = f"健康检查失败: {str(e)}\n{traceback.format_exc()}"
            
            self.send_response(500)
            self.send_header('Content-Type', 'text/plain')
            self.end_headers()
            self.wfile.write(error_msg.encode('utf-8'))
    
    def do_POST(self):
        if self.path == '/webhook/feishu/hktech':
            content_length = int(self.headers.get('Content-Length', 0))
            body = self.rfile.read(content_length).decode('utf-8')
            
            try:
                data = json.loads(body)
                event_type = data.get('header', {}).get('event_type', '')
                
                # URL验证
                if event_type == 'url_verification':
                    challenge = data.get('challenge', '')
                    self.send_response(200)
                    self.send_header('Content-Type', 'application/json')
                    self.end_headers()
                    self.wfile.write(json.dumps({'challenge': challenge}).encode())
                    return
                
                # 卡片点击事件 - 调用处理器
                if event_type == 'card.action.trigger':
                    import sys
                    sys.path.insert(0, '/opt/hktech-agent/prod/src')
                    from feishu_webhook_handler import transform
                    
                    result = transform({'body': body, 'headers': dict(self.headers)})
                    
                    self.send_response(200)
                    self.send_header('Content-Type', 'application/json')
                    self.end_headers()
                    self.wfile.write(json.dumps(result).encode())
                    return
                    
            except Exception as e:
                print(f"Webhook error: {e}")
                import traceback
                traceback.print_exc()
            
            self.send_response(200)
            self.send_header('Content-Type', 'application/json')
            self.end_headers()
            self.wfile.write(json.dumps({'code': 0}).encode())
        else:
            self.send_error(404)
    
    def end_headers(self):
        self.send_header('Access-Control-Allow-Origin', '*')
        super().end_headers()

if __name__ == '__main__':
    print(f"🚀 启动Web服务 on port {PORT}")
    print(f"📊 监控: http://localhost:{PORT}/")
    print(f"📚 文档: http://localhost:{PORT}/INVESTOR_PITCH.md")
    
    with socketserver.TCPServer(("", PORT), Handler) as httpd:
        httpd.serve_forever()
