#!/usr/bin/env node

const { execSync } = require('child_process');
const path = require('path');

/**
 * Test script to simulate the version update process
 * Usage: node test-update.js [version|tag]
 * Example: 
 *   node test-update.js 1.0.0
 *   node test-update.js v1.0.0
 */

function main() {
  const args = process.argv.slice(2);
  
  if (args.length !== 1) {
    console.error('Usage: node test-update.js [version|tag]');
    console.error('Examples:');
    console.error('  node test-update.js 1.0.0');
    console.error('  node test-update.js v1.0.0');
    process.exit(1);
  }
  
  const input = args[0];
  let version;
  
  try {
    // Check if input looks like a tag (starts with 'v') or a version
    if (input.startsWith('v')) {
      console.log(`🏷️  Processing tag: ${input}`);
      version = execSync(`node ${path.join(__dirname, 'extract-version.js')} ${input}`, { encoding: 'utf8' }).trim();
      console.log(`📝 Extracted version: ${version}`);
    } else {
      version = input;
      console.log(`📝 Using version: ${version}`);
    }
    
    // Update package.json files
    console.log(`🔄 Updating package.json files...`);
    execSync(`node ${path.join(__dirname, 'update-version.js')} ${version}`, { stdio: 'inherit' });
    
    console.log(`\n✅ Test completed successfully!`);
    console.log(`\n🚀 In the real workflow, this would be followed by:`);
    console.log(`   1. Building and packaging binaries`);
    console.log(`   2. Publishing to npm`);
    console.log(`   3. Creating GitHub release`);
    
  } catch (error) {
    console.error(`❌ Test failed:`, error.message);
    process.exit(1);
  }
}

if (require.main === module) {
  main();
}