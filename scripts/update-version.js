#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

/**
 * Update version in package.json files
 * Usage: node update-version.js <version>
 * Example: node update-version.js 0.2.0
 */

function updatePackageVersion(packagePath, version) {
  try {
    const packageJson = JSON.parse(fs.readFileSync(packagePath, 'utf8'));
    packageJson.version = version;
    
    // If this is the main package, also update optional dependencies
    if (packageJson.optionalDependencies) {
      for (const dep in packageJson.optionalDependencies) {
        if (dep.startsWith('@emacs-lsp-proxy/')) {
          packageJson.optionalDependencies[dep] = version;
        }
      }
    }
    
    fs.writeFileSync(packagePath, JSON.stringify(packageJson, null, 2) + '\n');
    console.log(`✅ Updated ${packagePath} to version ${version}`);
    return true;
  } catch (error) {
    console.error(`❌ Failed to update ${packagePath}:`, error.message);
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
  
  // List of package.json files to update
  const packageFiles = [
    'npm/emacs-lsp-proxy/package.json',
    'npm/@emacs-lsp-proxy/darwin-arm64/package.json',
    'npm/@emacs-lsp-proxy/darwin-x64/package.json',
    'npm/@emacs-lsp-proxy/linux-arm64/package.json',
    'npm/@emacs-lsp-proxy/linux-x64/package.json',
    'npm/@emacs-lsp-proxy/win32-x64/package.json'
  ];
  
  let success = true;
  
  for (const packageFile of packageFiles) {
    const fullPath = path.join(process.cwd(), packageFile);
    if (fs.existsSync(fullPath)) {
      if (!updatePackageVersion(fullPath, version)) {
        success = false;
      }
    } else {
      console.warn(`⚠️  Package file not found: ${fullPath}`);
    }
  }
  
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

module.exports = { updatePackageVersion };