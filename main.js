const { app, BrowserWindow, WebContentsView, Menu, ipcMain } = require('electron');
const { spawn, execSync } = require('child_process');
const fs = require('fs');
const path = require('path');
const http = require('http');
const net = require('net');

// Developpement : dossier du projet. Apres build : dossier "resources".
const base = app.isPackaged ? process.resourcesPath : __dirname;
const rscript = path.join(base, 'R-Portable', 'bin', 'Rscript.exe');
const rLib = path.join(base, 'R-Portable', 'library');

const TOOLBAR_H = 48;

// Les deux applications Shiny (code R inchange). Chacune tourne dans son
// propre processus R, demarre seulement quand on ouvre son onglet.
const APPS = [
  { id: 'achat', dir: 'shiny_achat', title: 'Contrôle Achat' },
  { id: 'stock', dir: 'shiny_stock', title: 'Contrôle Stock' }
];

let win = null;
let current = null;

// ---------------------------------------------------------------------------
// Outils
// ---------------------------------------------------------------------------
function freePort() {
  return new Promise((resolve) => {
    const srv = net.createServer();
    srv.listen(0, '127.0.0.1', () => {
      const p = srv.address().port;
      srv.close(() => resolve(p));
    });
  });
}

function waitForServer(port, proc, tries = 0) {
  return new Promise((resolve, reject) => {
    if (proc.exitCode !== null) {
      return reject(new Error("R s'est arrêté (code " + proc.exitCode + ')'));
    }
    if (tries > 240) return reject(new Error('Délai dépassé'));
    const req = http.get({ host: '127.0.0.1', port, path: '/', timeout: 2000 }, (res) => {
      res.resume();
      resolve();
    });
    req.on('error', () => {
      setTimeout(() => waitForServer(port, proc, tries + 1).then(resolve, reject), 500);
    });
    req.on('timeout', () => req.destroy());
  });
}

// Export PDF (pagedown) : a besoin de Chrome ou Edge, qu'on indique a R.
function findChromium() {
  const pf = process.env['ProgramFiles'] || 'C:\\Program Files';
  const pf86 = process.env['ProgramFiles(x86)'] || 'C:\\Program Files (x86)';
  const local = process.env['LOCALAPPDATA'] || '';
  const list = [
    path.join(pf, 'Google', 'Chrome', 'Application', 'chrome.exe'),
    path.join(pf86, 'Google', 'Chrome', 'Application', 'chrome.exe'),
    local && path.join(local, 'Google', 'Chrome', 'Application', 'chrome.exe'),
    path.join(pf86, 'Microsoft', 'Edge', 'Application', 'msedge.exe'),
    path.join(pf, 'Microsoft', 'Edge', 'Application', 'msedge.exe')
  ];
  return list.find((p) => p && fs.existsSync(p)) || null;
}

function esc(s) {
  return String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));
}

function htmlPage(body) {
  return 'data:text/html;charset=utf-8,' + encodeURIComponent(
    '<body style="font-family:Segoe UI,Arial;display:flex;align-items:center;' +
    'justify-content:center;height:100vh;margin:0;color:#333;text-align:center">' +
    body + '</body>');
}

function loadingPage(title) {
  return htmlPage('<div><h2>' + esc(title) + '</h2><p>Chargement en cours... جارٍ التحميل</p></div>');
}

function errorPage(title, msg) {
  return htmlPage(
    '<div style="max-width:700px"><h2 style="color:#c62828">' + esc(title) + '</h2>' +
    '<p>Le serveur R n\'a pas démarré. لم يبدأ خادم R.</p>' +
    '<pre style="text-align:left;white-space:pre-wrap;background:#f5f5f5;padding:10px">' + esc(msg) + '</pre>' +
    '<p>Cliquez de nouveau sur l\'onglet pour réessayer.</p></div>');
}

function status(id, state) {
  if (win && !win.isDestroyed()) win.webContents.send('status', { id, state });
}

// ---------------------------------------------------------------------------
// Demarrage d'une application Shiny (une seule fois, a la demande)
// ---------------------------------------------------------------------------
function startApp(a) {
  if (a.starting) return a.starting;

  a.starting = (async () => {
    status(a.id, 'loading');

    if (!a.view) {
      a.view = new WebContentsView({ webPreferences: { contextIsolation: true } });
      win.contentView.addChildView(a.view);
      a.view.setVisible(current === a.id);
      layout();
    }
    a.view.webContents.loadURL(loadingPage(a.title));

    let errTail = '';
    try {
      a.port = await freePort();
      const dir = path.join(base, a.dir);
      const dirR = dir.replace(/\\/g, '/');
      const env = { ...process.env, R_LIBS: rLib, R_LIBS_USER: rLib };
      const browser = findChromium();
      if (browser) env.PAGEDOWN_CHROME = browser;

      a.proc = spawn(
        rscript,
        ['-e', `shiny::runApp('${dirR}', port=${a.port}, host='127.0.0.1', launch.browser=FALSE)`],
        { cwd: dir, env, windowsHide: true }
      );
      a.proc.on('error', (e) => { errTail += String(e) + '\n'; });
      a.proc.stderr.on('data', (d) => {
        const t = String(d);
        console.log('[' + a.id + ']', t);
        errTail = (errTail + t).slice(-1500);
      });
      a.proc.on('exit', () => { status(a.id, a.ready ? 'stopped' : 'error'); });

      await waitForServer(a.port, a.proc);
      a.ready = true;
      a.view.webContents.loadURL(`http://127.0.0.1:${a.port}`);
      status(a.id, 'ready');
    } catch (e) {
      stopApp(a);
      a.view.webContents.loadURL(errorPage(a.title, (errTail || e.message).trim()));
      status(a.id, 'error');
      a.starting = null; // permet de reessayer
      a.ready = false;
    }
  })();

  return a.starting;
}

function stopApp(a) {
  if (a.proc && a.proc.pid) {
    try {
      if (process.platform === 'win32') execSync(`taskkill /pid ${a.proc.pid} /T /F`, { stdio: 'ignore' });
      else a.proc.kill();
    } catch (e) { /* deja arrete */ }
  }
  a.proc = null;
}

function stopAll() { APPS.forEach(stopApp); }

// ---------------------------------------------------------------------------
// Fenetre : barre d'onglets (shell.html) + une vue par application
// ---------------------------------------------------------------------------
function layout() {
  if (!win || win.isDestroyed()) return;
  const { width, height } = win.getContentBounds();
  APPS.forEach((a) => {
    if (a.view) a.view.setBounds({ x: 0, y: TOOLBAR_H, width, height: Math.max(0, height - TOOLBAR_H) });
  });
}

function show(id) {
  const a = APPS.find((x) => x.id === id);
  if (!a) return;
  current = id;
  APPS.forEach((x) => x.view && x.view.setVisible(x.id === id));
  win.webContents.send('active', id);
  startApp(a);
}

function createWindow() {
  Menu.setApplicationMenu(null);
  win = new BrowserWindow({
    width: 1400,
    height: 900,
    minWidth: 900,
    minHeight: 600,
    show: false,
    title: 'Controle Magasin',
    backgroundColor: '#ffffff',
    webPreferences: { preload: path.join(__dirname, 'preload.js'), contextIsolation: true }
  });
  win.loadFile(path.join(__dirname, 'shell.html'));
  win.once('ready-to-show', () => win.show());
  win.webContents.once('did-finish-load', () => show(APPS[0].id));
  ['resize', 'maximize', 'unmaximize', 'enter-full-screen', 'leave-full-screen']
    .forEach((ev) => win.on(ev, layout));
  win.on('closed', () => { win = null; });
}

// ---------------------------------------------------------------------------
// Cycle de vie
// ---------------------------------------------------------------------------
if (!app.requestSingleInstanceLock()) {
  app.quit();
} else {
  app.on('second-instance', () => {
    if (win) { if (win.isMinimized()) win.restore(); win.focus(); }
  });
  ipcMain.on('switch', (_e, id) => show(id));
  app.whenReady().then(createWindow);
  app.on('window-all-closed', () => { stopAll(); app.quit(); });
  app.on('before-quit', stopAll);
}
