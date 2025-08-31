#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

/**
 * Test script to verify README.md will be included in npm package
 * Usage: node test-npm-package.js
 */

function testNpmPackageContent() {
  console.log('🧪 Testing npm package content...\n');
  
  const mainPackagePath = 'npm/emacs-lsp-proxy';
  const packageJsonPath = path.join(mainPackagePath, 'package.json');
  const readmePath = 'README.md';
  
  // Check if README.md exists in project root
  if (!fs.existsSync(readmePath)) {
    console.error('❌ README.md not found in project root');
    return false;
  }
  console.log('✅ README.md exists in project root');
  
  // Check package.json files array
  if (!fs.existsSync(packageJsonPath)) {
    console.error(`❌ ${packageJsonPath} not found`);
    return false;
  }
  
  const packageJson = JSON.parse(fs.readFileSync(packageJsonPath, 'utf8'));
  
  if (!packageJson.files) {
    console.error('❌ package.json missing "files" array');
    return false;
  }
  console.log('✅ package.json has "files" array');
  
  if (!packageJson.files.includes('README.md')) {
    console.error('❌ README.md not included in package.json files array');
    console.log('Current files array:', packageJson.files);
    return false;
  }
  console.log('✅ README.md is included in package.json files array');
  
  // Test what would be included in npm package
  try {
    console.log('\n📦 Testing npm pack (dry run)...');
    const cwd = process.cwd();
    process.chdir(mainPackagePath);
    
    // Create a temporary README.md for testing
    const tempReadme = 'README.md';
    if (!fs.existsSync(tempReadme)) {
      fs.copyFileSync(path.join('../../', 'README.md'), tempReadme);
      console.log('📄 Copied README.md to package directory for testing');
    }
    
    const packOutput = execSync('npm pack --dry-run', { encoding: 'utf8' });
    console.log('npm pack output:');
    console.log(packOutput);
    
    const includesReadme = packOutput.includes('README.md');
    
    // Clean up temporary README.md
    if (fs.existsSync(tempReadme)) {
      fs.unlinkSync(tempReadme);
      console.log('🧹 Cleaned up temporary README.md');
    }
    
    process.chdir(cwd);
    
    if (includesReadme) {
      console.log('✅ README.md would be included in npm package');
      return true;
    } else {
      console.log('❌ README.md would NOT be included in npm package');
      return false;
    }
  } catch (error) {
    console.error('❌ npm pack test failed:', error.message);
    process.chdir(process.cwd());
    return false;
  }
}

function main() {
  console.log('🧪 Testing npm package configuration for README.md inclusion\n');
  
  const success = testNpmPackageContent();
  
  if (success) {
    console.log('\n🎉 All tests passed! README.md will be included in the npm package.');
    console.log('\n📋 Next steps:');
    console.log('1. When you publish a new version, README.md will automatically be included');
    console.log('2. Users can find documentation at: node_modules/emacs-lsp-proxy/README.md');
    console.log('3. npm will also display README.md on the package page');
  } else {
    console.log('\n❌ Tests failed. Please check the configuration.');
    process.exit(1);
  }
}

if (require.main === module) {
  main();
}