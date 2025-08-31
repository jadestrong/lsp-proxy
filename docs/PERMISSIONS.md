# 自动版本提交的权限配置

如果你在使用这个 workflow 时遇到权限问题，可能需要调整以下设置：

## GitHub Actions 权限

在你的仓库设置中，确保 GitHub Actions 有写入权限：

1. 进入仓库 Settings
2. 点击 Actions -> General
3. 在 "Workflow permissions" 部分选择 "Read and write permissions"
4. 勾选 "Allow GitHub Actions to create and approve pull requests"

## 替代方案：使用 Personal Access Token

如果你希望使用更精细的权限控制，可以使用 Personal Access Token：

1. 创建 Personal Access Token (classic) 包含以下权限：
   - `repo` (完整仓库访问权限)
   - `write:packages` (如果需要发布 packages)

2. 将 token 添加到仓库 Secrets，命名为 `RELEASE_TOKEN`

3. 修改 workflow 中的 git 配置部分：

```yaml
- name: Commit version updates
  env:
    GITHUB_TOKEN: ${{ secrets.RELEASE_TOKEN }}
  run: |
    git config --local user.email "action@github.com"
    git config --local user.name "GitHub Action"
    git remote set-url origin https://x-access-token:${GITHUB_TOKEN}@github.com/${{ github.repository }}
    git add npm/*/package.json
    if git diff --staged --quiet; then
      echo "No changes to commit"
    else
      git commit -m "release: update package versions to ${{ steps.extract_version.outputs.version }}"
      git push origin HEAD:main
    fi
```

## 分支保护规则

如果你的 main 分支有保护规则，你可能需要：

1. 在分支保护规则中添加 GitHub Actions 作为例外
2. 或者允许管理员推送（如果你的 token 有管理员权限）
3. 或者使用 Pull Request 方式而不是直接推送