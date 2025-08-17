#!/usr/bin/env node

/**
 * Extract version from git tag
 * Usage: node extract-version.js <tag>
 * Example: node extract-version.js v1.0.0 -> 1.0.0
 */

function extractVersion(tag) {
  // Remove 'v' prefix if it exists
  const version = tag.replace(/^v/, '');
  
  // Validate version format
  if (!/^\d+\.\d+\.\d+(-[a-zA-Z0-9\.-]+)?$/.test(version)) {
    throw new Error(`Invalid version format: ${version}`);
  }
  
  return version;
}

function main() {
  const args = process.argv.slice(2);
  
  if (args.length !== 1) {
    console.error('Usage: node extract-version.js <tag>');
    console.error('Example: node extract-version.js v1.0.0');
    process.exit(1);
  }
  
  try {
    const version = extractVersion(args[0]);
    console.log(version);
  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
}

if (require.main === module) {
  main();
}

module.exports = { extractVersion };