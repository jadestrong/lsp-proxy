#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process'); // 添加这行

/**
 * Update version in package.json files
 * Usage: node update-version.js <version>
 * Example: node update-version.js 0.2.0
 */

function updatePackageVersion(packagePath, version) {
  try {
    console.log(`🔍 Debug: Processing file ${packagePath}`);
    
    if (!require('fs').existsSync(packagePath)) {
      console.error(`❌ File does not exist: ${packagePath}`);
      return false;
    }
    
    const packageJson = JSON.parse(fs.readFileSync(packagePath, 'utf8'));
    const oldVersion = packageJson.version;
    console.log(`📄 Current version in ${packagePath}: ${oldVersion}`);
    
    if (oldVersion === version) {
      console.log(`⚠️  Version already matches in ${packagePath}: ${version}`);
      return true; // Still consider this a success
    }
    
    packageJson.version = version;
    
    // If this is the main package, also update optional dependencies
    if (packageJson.optionalDependencies) {
      console.log(`🔗 Updating optional dependencies in ${packagePath}`);
      for (const dep in packageJson.optionalDependencies) {
        if (dep.startsWith('@emacs-lsp-proxy/')) {
          const oldDepVersion = packageJson.optionalDependencies[dep];
          packageJson.optionalDependencies[dep] = version;
          console.log(`   ${dep}: ${oldDepVersion} → ${version}`);
        }
      }
    }
    
    // Ensure files array includes README.md for main package
    if (packageJson.name === 'emacs-lsp-proxy' && packageJson.files) {
      if (!packageJson.files.includes('README.md')) {
        packageJson.files.push('README.md');
        console.log(`📄 Added README.md to files list in ${packagePath}`);
      }
    }
    
    fs.writeFileSync(packagePath, JSON.stringify(packageJson, null, 2) + '\n');
    console.log(`✅ Updated ${packagePath}: ${oldVersion} → ${version}`);
    return true;
  } catch (error) {
    console.error(`❌ Failed to update ${packagePath}:`, error.message);
    return false;
  }
}

function updateCargoVersion(cargoPath, version) {
  try {
    console.log(`🔍 Debug: Processing Cargo file ${cargoPath}`);
    
    if (!fs.existsSync(cargoPath)) {
      console.error(`❌ Cargo.toml file does not exist: ${cargoPath}`);
      return false;
    }
    
    let content = fs.readFileSync(cargoPath, 'utf8');
    const versionRegex = /^version\s*=\s*"([^"]+)"/m;
    const match = content.match(versionRegex);
    
    if (!match) {
      console.error(`❌ Could not find version field in ${cargoPath}`);
      return false;
    }
    
    const oldVersion = match[1];
    if (oldVersion === version) {
      console.log(`⚠️  Version already matches in ${cargoPath}: ${version}`);
      return true;
    }
    
    content = content.replace(versionRegex, `version = "${version}"`);
    fs.writeFileSync(cargoPath, content);
    console.log(`✅ Updated ${cargoPath}: ${oldVersion} → ${version}`);
    return true;
  } catch (error) {
    console.error(`❌ Failed to update ${cargoPath}:`, error.message);
    return false;
  }
}

function updateCargoLock(cargoDir) {
  try {
    console.log(`🔄 Updating Cargo.lock in ${cargoDir}...`);
    
    // 检查是否存在 Cargo.toml
    const cargoTomlPath = path.join(cargoDir, 'Cargo.toml');
    if (!fs.existsSync(cargoTomlPath)) {
      console.warn(`⚠️  No Cargo.toml found in ${cargoDir}, skipping Cargo.lock update`);
      return true;
    }
    
    // 运行 cargo update 命令
    execSync('cargo update', { 
      cwd: cargoDir, 
      stdio: 'inherit' // 显示 cargo 的输出
    });
    
    console.log(`✅ Successfully updated Cargo.lock in ${cargoDir}`);
    return true;
  } catch (error) {
    console.error(`❌ Failed to update Cargo.lock in ${cargoDir}:`, error.message);
    return false;
  }
}

function main() {
  const args = process.argv.slice(2);
  
  if (args.length !== 1) {
    console.error('Usage: node update-version.js <version>');
    console.error('Example: node update-version.js 0.2.0');
    process.exit(1);
  }
  
  const version = args[0];
  
  // Validate version format (basic semver check)
  if (!/^\d+\.\d+\.\d+(-[a-zA-Z0-9\.-]+)?$/.test(version)) {
    console.error('❌ Invalid version format. Please use semantic versioning (e.g., 1.0.0)');
    process.exit(1);
  }
  
  console.log(`🔄 Updating all package.json files to version ${version}...`);
  console.log(`🔍 Debug: Current working directory: ${process.cwd()}`);
  
  // List of package.json files to update
  const packageFiles = [
    'npm/emacs-lsp-proxy/package.json',
    'npm/@emacs-lsp-proxy/darwin-arm64/package.json',
    'npm/@emacs-lsp-proxy/darwin-x64/package.json',
    'npm/@emacs-lsp-proxy/linux-arm64/package.json',
    'npm/@emacs-lsp-proxy/linux-x64/package.json',
    'npm/@emacs-lsp-proxy/win32-x64/package.json'
  ];

  const cargoFiles = [
    'Cargo.toml'  // 添加你需要更新的 Cargo.toml 文件路径
  ];
  
  console.log(`🔍 Debug: Files to process:`, [...packageFiles, ...cargoFiles]);
  
  let success = true;
  let processedCount = 0;
  let updatedCount = 0;
  
  // Process package.json files
  for (const packageFile of packageFiles) {
    const fullPath = path.join(process.cwd(), packageFile);
    console.log(`\n🔍 Debug: Checking ${packageFile} (full path: ${fullPath})`);
    
    if (fs.existsSync(fullPath)) {
      console.log(`✅ File exists: ${packageFile}`);
      processedCount++;
      if (updatePackageVersion(fullPath, version)) {
        updatedCount++;
      } else {
        success = false;
      }
    } else {
      console.warn(`⚠️  Package file not found: ${fullPath}`);
    }
  }

  // Process Cargo.toml files
  const cargoDirectories = new Set(); // 收集需要更新 Cargo.lock 的目录
  
  for (const cargoFile of cargoFiles) {
    const fullPath = path.join(process.cwd(), cargoFile);
    console.log(`\n🔍 Debug: Checking ${cargoFile} (full path: ${fullPath})`);
    
    if (fs.existsSync(fullPath)) {
      console.log(`✅ File exists: ${cargoFile}`);
      processedCount++;
      if (updateCargoVersion(fullPath, version)) {
        updatedCount++;
        // 添加包含 Cargo.toml 的目录到集合中
        cargoDirectories.add(path.dirname(fullPath));
      } else {
        success = false;
      }
    } else {
      console.warn(`⚠️  Cargo file not found: ${fullPath}`);
    }
  }
  
  // 更新所有相关的 Cargo.lock 文件
  if (cargoDirectories.size > 0) {
    console.log(`\n🔄 Updating Cargo.lock files...`);
    for (const cargoDir of cargoDirectories) {
      if (!updateCargoLock(cargoDir)) {
        success = false;
      }
    }
  }
  
  console.log(`\n📊 Summary: Processed ${processedCount}/${packageFiles.length + cargoFiles.length} files, updated ${updatedCount} files`);
  
  if (success) {
    console.log(`🎉 Successfully updated all package.json files to version ${version}`);
  } else {
    console.error('❌ Some files failed to update');
    process.exit(1);
  }
}

if (require.main === module) {
  main();
}

module.exports = { updatePackageVersion, updateCargoVersion };
