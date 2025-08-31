# 快速开始：自动化版本管理

## 一次性设置

1. **确保权限配置**
   - 进入仓库 Settings -> Actions -> General
   - 选择 "Read and write permissions"
   - 勾选 "Allow GitHub Actions to create and approve pull requests"

2. **验证脚本工作正常**
   ```bash
   # 本地测试
   npm run test-update v1.0.0
   ```

## 日常使用流程

### 发布新版本（完全自动化）

```bash
# 1. 完成开发工作
git add .
git commit -m "feat: your changes"
git push

# 2. 创建并推送版本标签
git tag v1.0.0
git push origin v1.0.0

# 3. 等待 GitHub Actions 完成（约 10-15 分钟）
# ✅ 自动更新所有 package.json
# ✅ 自动提交版本更新
# ✅ 自动构建所有平台二进制文件
# ✅ 自动发布到 npm
# ✅ 自动创建 GitHub Release
```

### 检查发布状态

1. 查看 GitHub Actions: `https://github.com/YOUR_USERNAME/lsp-proxy/actions`
2. 检查 npm 包: `https://www.npmjs.com/package/emacs-lsp-proxy`
3. 查看 GitHub Release: `https://github.com/YOUR_USERNAME/lsp-proxy/releases`

## 常用命令

```bash
# 本地测试版本更新
npm run test-update v1.2.3

# 手动更新版本（如果需要）
npm run update-version 1.2.3

# 提取版本号
npm run extract-version v1.2.3  # 输出: 1.2.3
```

## 支持的版本格式

- `v1.0.0` → `1.0.0`
- `v1.0.0-alpha.1` → `1.0.0-alpha.1`
- `v1.0.0-beta.2` → `1.0.0-beta.2`
- `v1.0.0-rc.1` → `1.0.0-rc.1`

## 故障排除

### 发布失败？
1. 检查 GitHub Actions 日志
2. 确认 NPM_TOKEN 正确设置
3. 验证版本号格式
4. 检查权限配置

### 版本提交失败？
1. 检查分支保护规则
2. 确认 GitHub Actions 有写入权限
3. 查看 [权限配置文档](PERMISSIONS.md)

现在你可以完全自动化地发布新版本了！🚀