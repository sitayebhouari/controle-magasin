# Controle Magasin — تطبيق سطح مكتب واحد (Achat + Stock)

نافذة Electron واحدة فيها تبويبان:

| التبويب | المجلد | صيغة ملف الإكسل |
|---|---|---|
| Contrôle Achat | `shiny_achat/` | ورقة `MOUVEMENT`، أعمدة `ENTRIE` و`SORTEE` و`REF PRODUIT`، والترويسة في السطر 1 |
| Contrôle Stock | `shiny_stock/` | كمية موقّعة `QUANTITE`، وسطر الترويسة يُكتشف تلقائياً (الصيغة الحالية) |

كود R في المجلدين **لم يُعدَّل**. كل تطبيق يعمل في عملية R مستقلة تبدأ عند فتح تبويبه فقط،
ويبقى الملف المحمّل في كل تبويب عند التبديل بينهما.

## المتطلبات (مرة واحدة)
- R مثبّت في `C:\Program Files\R\R-x.y.z`
- Node.js (LTS)

## الخطوات
1. فك الضغط في مجلد مسارُه بدون أحرف عربية أو مسافات إن أمكن، مثل `C:\controle-magasin`.
2. افتح PowerShell داخل المجلد ونفّذ:
   `powershell -ExecutionPolicy Bypass -File .\setup.ps1`
   (يتحقق من الملفات، وينسخ R إلى `R-Portable`، ويثبّت الحزم، ثم `npm install`. مدته 5 إلى 15 دقيقة.)
3. للتجربة: `npm start`
4. لبناء المثبّت: `npm run build` — يظهر في `dist\Controle Magasin-Setup-1.0.0.exe`

## ملاحظات
- **تصدير PDF** (في Contrôle Achat) يحتاج Google Chrome أو Microsoft Edge على الجهاز.
  التطبيق يبحث عنهما ويمرّر المسار إلى R تلقائياً.
- لتغيير اسم التطبيق أو معرّفه عدّل `productName` و`appId` في `package.json`.
- حجم المثبّت كبير (مئات الميغابايت) لأنه يتضمن R وحزمه.
