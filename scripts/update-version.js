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
  
  console.log(`🔍 Debug: Files to process:`, packageFiles);
  
  let success = true;
  let processedCount = 0;
  let updatedCount = 0;
  
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
  
  console.log(`\n📊 Summary: Processed ${processedCount}/${packageFiles.length} files, updated ${updatedCount} files`);
  
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
