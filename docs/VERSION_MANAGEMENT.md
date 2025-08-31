# 自动化版本管理和发布指南

## 概述

这个项目现在支持通过 Git tag 自动更新所有 npm package.json 文件的版本号，并完成发布流程。

## 工作流程

### 1. 自动化发布（推荐）

1. **准备发布**
   ```bash
   # 确保所有更改都已提交
   git add .
   git commit -m "feat: prepare for release"
   git push
   ```

2. **创建并推送版本标签**
   ```bash
   # 创建新版本标签 (例如 v0.2.0)
   git tag v0.2.0
   git push origin v0.2.0
   ```

3. **自动化流程**
   当你推送 tag 时，GitHub Actions 会自动：
   - 从 tag 提取版本号 (v0.2.0 → 0.2.0)
   - 更新所有 package.json 文件的版本号
   - **自动提交版本更新到 main 分支** (commit 消息: `release: update package versions to 0.2.0`)
   - 构建所有平台的二进制文件
   - 发布到 npm
   - 创建 GitHub Release

### 2. 本地测试（开发用）

在推送 tag 前，你可以本地测试版本更新：

```bash
# 测试版本号提取和更新
npm run test-update v0.2.0

# 或者直接使用版本号
npm run test-update 0.2.0

# 手动更新版本（如果需要）
npm run update-version 0.2.0
```

## 脚本说明

### scripts/extract-version.js
从 Git tag 中提取版本号。

```bash
node scripts/extract-version.js v1.0.0  # 输出: 1.0.0
node scripts/extract-version.js v1.0.0-beta.1  # 输出: 1.0.0-beta.1
```

### scripts/update-version.js
更新所有 package.json 文件的版本号。

```bash
node scripts/update-version.js 1.0.0
```

更新的文件：
- `npm/emacs-lsp-proxy/package.json` - 主包和所有依赖版本
- `npm/@emacs-lsp-proxy/darwin-arm64/package.json`
- `npm/@emacs-lsp-proxy/darwin-x64/package.json`
- `npm/@emacs-lsp-proxy/linux-arm64/package.json`
- `npm/@emacs-lsp-proxy/linux-x64/package.json`
- `npm/@emacs-lsp-proxy/win32-x64/package.json`

### scripts/test-update.js
本地测试完整的版本更新流程。

```bash
node scripts/test-update.js v1.0.0
```

## Git 提交历史

采用这个自动化流程后，你的 Git 历史将会是这样的：

```
* a1b2c3d (HEAD -> main, origin/main) release: update package versions to 0.2.0
* d4e5f6g feat: prepare for release  
* g7h8i9j fix: some bug fixes
* j0k1l2m feat: add new feature
* (tag: v0.2.0)
```

**说明**：
- 当你推送 `v0.2.0` tag 时，GitHub Actions 会自动创建一个新的 commit 来更新版本号
- 这个 commit 会被推送到 main 分支
- 这样可以保持 package.json 文件的版本号与 Git tag 同步
- Release commit 的格式：`release: update package versions to <version>`

## 版本号格式

支持标准的语义化版本号格式：
- `1.0.0` - 标准版本
- `1.0.0-alpha.1` - 预发布版本
- `1.0.0-beta.2` - Beta 版本
- `1.0.0-rc.1` - 候选版本

## 发布检查清单

发布新版本前，请确认：

1. ✅ 所有测试通过
2. ✅ CHANGELOG 已更新
3. ✅ 所有更改已提交并推送
4. ✅ 版本号遵循语义化版本规范
5. ✅ 已在本地测试版本更新脚本

## 常见问题

### Q: 如果版本更新失败怎么办？
A: 检查 GitHub Actions 日志，常见原因：
- 版本号格式不正确
- npm 权限问题
- 网络连接问题

### Q: 如何撤销已发布的版本？
A: npm 包发布后不建议撤销，建议发布新的修复版本。

### Q: 如何发布预发布版本？
A: 使用带有预发布标识的 tag：
```bash
git tag v1.0.0-beta.1
git push origin v1.0.0-beta.1
```

### Q: 本地 package.json 被意外修改了怎么办？
A: 使用 git 重置：
```bash
git checkout -- npm/*/package.json
```

## 旧的手动流程（不推荐）

如果需要手动更新，仍可以：
1. 手动编辑每个 package.json 文件
2. 手动运行构建和发布命令

但强烈建议使用新的自动化流程。