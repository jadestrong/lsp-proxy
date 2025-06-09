const child_process = require('child_process');
const zlib = require('zlib');
const https = require('https');
const fs = require('fs');
const os = require('os');
const path = require('path');

const versionFromPackageJSON = require(path.join(__dirname, 'package.json')).version;
const toPath = path.join(__dirname, 'bin', 'emasc-lsp-proxy')
// let isToPathJS = true;

const knownWindowsPackages = {
    'win32 x64 LE': '@emacs-lsp-proxy/win32-x64',
};

const knownUnixlikePackages = {
    'darwin arm64 LE': '@emacs-lsp-proxy/darwin-arm64',
    'darwin x64 LE': '@emacs-lsp-proxy/darwin-x64',
    'linux arm64 LE': '@emacs-lsp-proxy/linux-arm64',
    'linux x64 LE': '@emacs-lsp-proxy/linux-x64',
};

function pkgAndSubpathforCurrentPlatform() {
    let pkg;
    let subpath;

    let platformKey = `${process.platform} ${os.arch()} ${os.endianness()}`;

    if (platformKey in knownWindowsPackages) {
        pkg = knownWindowsPackages[platformKey];
        subpath = 'emacs-lsp-proxy.exe';
    }

    else if (platformKey in knownUnixlikePackages) {
        pkg = knownUnixlikePackages[platformKey];
        subpath = 'bin/emacs-lsp-proxy';
    }

    else {
        throw new Error(`Unsupported platform: ${platformKey}`);
    }

    return {pkg, subpath};
}

async function checkAndPreparePackage() {
    const { pkg, subpath } = pkgAndSubpathforCurrentPlatform();

    let binPath;
    try {
        binPath = require.resolve(`${pkg}/${subpath}`);
    } catch (e) {
        binPath = downloadedBinPath(pkg, subpath);
        try {
            console.error(`[emacs-lsp-proxy] Attempting to download package "${pkg}" using npm`);
            installingUsingNpm(pkg, subpath, binPath);
        } catch (e) {
            console.error(`[emacs-lsp-proxy] Failed to download package "${pkg}" using npm`);
            try {
                await downloadDirectlyFromNPM(pkg, subpath, binPath);
            } catch (e3) {
                throw new Error(`Failed to download package "${pkg}"`);
            }
        }
    }

    maybeOptimizePackage(binPath);
}

function installingUsingNpm(pkg, subpath, binPath) {
    const env = { ...process.env, npm_config_global: undefined };

    const proxyLibDir = path.dirname(require.resolve('emacs-lsp-proxy'));
    const installDir = path.join(proxyLibDir, 'npm-install');
    fs.mkdirSync(installDir);
    try {
        fs.writeFileSync(path.join(installDir, 'package.json'), '{}');
        child_process.execSync(`npm install --loglevel=error --prefer-offline --no-audit --progress=false ${pkg}@${versionFromPackageJSON}`, {
            cwd: installDir,
            stdio: 'pipe',
            env
        });
        const installedBinPath = path.join(installDir, 'node_modules', pkg, subpath);
        fs.renameSync(installedBinPath, binPath);
    } catch (e) {
        // Ignore
    } finally {
        try {
            removeRecursive(installDir);
        } catch (e) {
            // Ignore
        }
    }
}

function removeRecursive(dir) {
    for (const entry of fs.readdirSync(dir)) {
        const entryPath = path.join(dir, entry);
        let stats;
        try {
            stats = fs.lstatSync(entryPath);
        } catch (e) {
            continue;
        }
        if (stats.isDirectory()) {
            removeRecursive(entryPath);
        } else {
            fs.unlinkSync(entryPath);
        }
        fs.rmdirSync(dir);
    }
}

async function downloadDirectlyFromNPM(pkg, subpath, binPath) {
    const url = `https://registry.npmjs.org/${pkg}/-${pkg.replace('@emacs-lsp-proxy/', '')}-${versionFromPackageJSON}.tgz`;
    console.error(`[emacs-lsp-proxy] Trying to download "${pkg}" from ${url}`);
    try {
        fs.writeFileSync(binPath, extractFileFromTarGzip(await fetch(url), subpath));
        fs.chmodSync(binPath, 0o755);
    } catch (e) {
        console.error(`[emacs-lsp-proxy] Failed to download "${pkg}" from ${url}`);
        throw e;
    }
}

function downloadedBinPath(pkg, subpath) {
    const proxyLibDir = path.dirname(require.resolve('emacs-lsp-proxy'));
    return path.join(proxyLibDir, `downloaded-${pkg.replace('/', '-')}-${subpath.replace('/', '-')}-${path.basename(subpath)}`);
}

function maybeOptimizePackage(binPath) {
    if (os.platform() !== 'win32' && !isYarn()) {
        const tempPath = path.join(__dirname, 'bin-emacs-lsp-proxy');
        try {
            fs.linkSync(binPath, tempPath);
            fs.renameSync(tempPath, toPath);
            isToPathJS = false;
            fs.unlinkSync(tempPath);
        } catch (e) {
            // Ignore
        }
    }
}

function isYarn() {
  const { npm_config_user_agent } = process.env
  if (npm_config_user_agent) {
    return /\byarn\//.test(npm_config_user_agent)
  }
  return false
}

function fetch(url) {
  return new Promise((resolve, reject) => {
    https.get(url, res => {
      if ((res.statusCode === 301 || res.statusCode === 302) && res.headers.location)
        return fetch(res.headers.location).then(resolve, reject)
      if (res.statusCode !== 200)
        return reject(new Error(`Server responded with ${res.statusCode}`))
      let chunks = []
      res.on('data', chunk => chunks.push(chunk))
      res.on('end', () => resolve(Buffer.concat(chunks)))
    }).on('error', reject)
  })
}

function extractFileFromTarGzip(buffer, subpath) {
  try {
    buffer = zlib.unzipSync(buffer)
  } catch (err) {
    throw new Error(`Invalid gzip data in archive: ${err && err.message || err}`)
  }
  let str = (i, n) => String.fromCharCode(...buffer.subarray(i, i + n)).replace(/\0.*$/, '')
  let offset = 0
  subpath = `package/${subpath}`
  while (offset < buffer.length) {
    let name = str(offset, 100)
    let size = parseInt(str(offset + 124, 12), 8)
    offset += 512
    if (!isNaN(size)) {
      if (name === subpath) return buffer.subarray(offset, offset + size)
      offset += (size + 511) & ~511
    }
  }
  throw new Error(`Could not find ${JSON.stringify(subpath)} in archive`)
}


checkAndPreparePackage().then(() => {
    console.log('[emacs-lsp-proxy] Done');
})
