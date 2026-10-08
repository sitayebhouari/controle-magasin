const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('shell', {
  switchTo: (id) => ipcRenderer.send('switch', id),
  onActive: (cb) => ipcRenderer.on('active', (_e, id) => cb(id)),
  onStatus: (cb) => ipcRenderer.on('status', (_e, s) => cb(s))
});
