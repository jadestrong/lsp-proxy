const fs = require('fs');
const path = require('path');
const os = require('os');

// Platform-specific package mappings
const platformPackages = {
  'darwin-arm64': '@emacs-lsp-proxy/darwin-arm64',
  'darwin-x64': '@emacs-lsp-proxy/darwin-x64', 
  'linux-arm64': '@emacs-lsp-proxy/linux-arm64',
  'linux-x64': '@emacs-lsp-proxy/linux-x64',
  'win32-x64': '@emacs-lsp-proxy/win32-x64'
};

function getPlatformKey() {
  const platform = os.platform();
  const arch = os.arch();
  
  // Normalize architecture names
  const normalizedArch = arch === 'x64' ? 'x64' : 
                        arch === 'arm64' ? 'arm64' : arch;
  
  return `${platform}-${normalizedArch}`;
}

function findBinary() {
  const platformKey = getPlatformKey();
  const packageName = platformPackages[platformKey];
  
  if (!packageName) {
    console.error(`[emacs-lsp-proxy] Unsupported platform: ${platformKey}`);
    process.exit(1);
  }

  try {
    // Try to resolve the platform-specific package
    const packagePath = require.resolve(packageName + '/package.json');
    const packageDir = path.dirname(packagePath);
    
    // Look for binary in expected locations
    const binaryName = platformKey.startsWith('win32') ? 'emacs-lsp-proxy.exe' : 'emacs-lsp-proxy';
    const possiblePaths = [
      path.join(packageDir, binaryName),
      path.join(packageDir, 'bin', binaryName)
    ];
    
    for (const binPath of possiblePaths) {
      if (fs.existsSync(binPath)) {
        // Create symlink or copy to expected location
        const targetPath = path.join(__dirname, 'bin', 'emacs-lsp-proxy' + (platformKey.startsWith('win32') ? '.exe' : ''));
        
        fs.mkdirSync(path.dirname(targetPath), { recursive: true });
        
        try {
          // Try to create a hard link first (faster)
          if (fs.existsSync(targetPath)) fs.unlinkSync(targetPath);
          fs.linkSync(binPath, targetPath);
        } catch (e) {
          // Fall back to copying
          fs.copyFileSync(binPath, targetPath);
        }
        
        // Make executable on Unix systems
        if (!platformKey.startsWith('win32')) {
          fs.chmodSync(targetPath, 0o755);
        }
        
        console.log(`[emacs-lsp-proxy] Binary installed successfully for ${platformKey}`);
        return;
      }
    }
    
    console.error(`[emacs-lsp-proxy] Binary not found in package ${packageName}`);
    process.exit(1);
    
  } catch (e) {
    console.error(`[emacs-lsp-proxy] Platform package ${packageName} not found. This usually means the package wasn't installed properly.`);
    console.error('Make sure your platform is supported and npm installed the optional dependencies.');
    process.exit(1);
  }
}

// Only run if this is the main package being installed
if (require.main === module) {
  findBinary();
}