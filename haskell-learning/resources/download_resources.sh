#!/bin/bash
# 下载 Haskell 学习资源脚本
# 只下载合法免费的资源

RESOURCES_DIR="$(dirname "$0")"
cd "$RESOURCES_DIR" || exit 1

echo "=========================================="
echo "Haskell 学习资源下载脚本"
echo "=========================================="
echo ""

# 函数：下载文件
download_file() {
    local url=$1
    local filename=$2
    local description=$3
    
    echo "📥 下载: $description"
    if [ -f "$filename" ]; then
        echo "   ⚠️  文件已存在: $filename"
        read -p "   是否重新下载? (y/n) " -n 1 -r
        echo
        if [[ ! $REPLY =~ ^[Yy]$ ]]; then
            echo "   ⏭️  跳过"
            return
        fi
        rm -f "$filename"
    fi
    
    if curl -L -o "$filename" "$url" --progress-bar; then
        local size=$(du -h "$filename" | cut -f1)
        echo "   ✅ 完成 ($size)"
    else
        echo "   ❌ 下载失败"
        rm -f "$filename"
    fi
    echo ""
}

# 1. 下载 Category Theory for Programmers
echo "📚 Category Theory for Programmers"
echo "----------------------------------------"
echo "请选择版本:"
echo "1) PDF 版 (原版 Haskell)"
echo "2) PDF 版 (Scala 版)"
echo "3) 跳过"
read -p "选择 (1-3): " choice

case $choice in
    1)
        echo "正在获取最新版本链接..."
        # 尝试从 GitHub API 获取最新 release
        LATEST_URL=$(curl -s https://api.github.com/repos/hmemcpy/milewski-ctfp-pdf/releases/latest | grep "browser_download_url.*ctfp.pdf" | cut -d '"' -f 4)
        if [ -n "$LATEST_URL" ]; then
            download_file "$LATEST_URL" "category-theory-for-programmers.pdf" "Category Theory for Programmers PDF"
        else
            echo "❌ 无法获取下载链接"
            echo "💡 请手动访问: https://github.com/hmemcpy/milewski-ctfp-pdf/releases"
        fi
        ;;
    2)
        echo "正在获取 Scala 版链接..."
        LATEST_URL=$(curl -s https://api.github.com/repos/hmemcpy/milewski-ctfp-pdf/releases/latest | grep "browser_download_url.*ctfp-scala.pdf" | cut -d '"' -f 4)
        if [ -n "$LATEST_URL" ]; then
            download_file "$LATEST_URL" "category-theory-for-programmers-scala.pdf" "Category Theory for Programmers (Scala版) PDF"
        else
            echo "❌ 无法获取下载链接"
        fi
        ;;
    *)
        echo "⏭️  跳过"
        ;;
esac

echo ""
echo "=========================================="
echo "其他资源链接（请手动访问）"
echo "=========================================="
echo ""
echo "📖 免费在线书籍:"
echo "   • Learn You a Haskell: https://learnyouahaskell.github.io/"
echo "   • Real World Haskell: http://book.realworldhaskell.org/"
echo "   • What I Wish I Knew When Learning Haskell: http://dev.stephendiehl.com/hask/"
echo ""
echo "📚 学术论文（免费）:"
echo "   • Lambda Calculus - Stanford Encyclopedia: https://plato.stanford.edu/entries/lambda-calculus/"
echo "   • Type Theory and Functional Programming: https://www.cs.kent.ac.uk/people/staff/sjt/TTFP/"
echo ""
echo "🎓 课程材料:"
echo "   • CIS 194 (UPenn): http://www.seas.upenn.edu/~cis194/spring13/lectures.html"
echo ""
echo "💰 需要购买的经典书籍:"
echo "   • Types and Programming Languages (TAPL) - Benjamin C. Pierce"
echo "   • 购买链接: https://www.amazon.com/Types-Programming-Languages-MIT-Press/dp/0262162091"
echo ""
echo "=========================================="
echo "下载完成！"
echo "=========================================="
