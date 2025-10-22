## Installation (Required Steps)

### Step 1: Configure Your UIDs (REQUIRED - 10 minutes)

1. **Extract** the `dhis2-app` folder
2. **Open** `index.html` in a text editor (Notepad++, VS Code, etc.)

3. **Find Data Elements** (around line 20):
```javascript
const DHIS2_DATA_ELEMENTS = {
    'suspected': 'YOUR_UID_HERE',   // Replace with YOUR UID
    'confirmed': 'YOUR_UID_HERE',   // Replace with YOUR UID
    'tests': 'YOUR_UID_HERE',       // Replace with YOUR UID
    ...
};
```

4. **Find Organization Units** (around line 60):
```javascript
const DHIS2_ORG_UNITS = {
    'national': 'YOUR_UID_HERE',    // Replace with YOUR UID
    'Bo': 'YOUR_UID_HERE',          // Replace with YOUR UID
    ...
};
```

5. **Replace ALL UIDs** with your actual DHIS2 data element and organization unit UIDs

**Use [UID-MAPPING-TEMPLATE.md](UID-MAPPING-TEMPLATE.md) to organize your UIDs first!**

### Step 2: Create ZIP Package

1. Make sure you're in the parent folder
2. Select the `dhis2-app` folder (the folder, not the files inside)
3. Right-click → Compress/Create Archive
4. Name it: `malaria-analytics-dashboard.zip`

### Step 3: Upload to DHIS2

1. Log into DHIS2 as administrator
2. Go to **App Management**
3. Click **Install App**
4. Upload `malaria-analytics-dashboard.zip`
5. Wait for installation

### Step 4: Open & Use

1. Go to **Apps** menu
2. Click **Malaria Analytics Dashboard**
3. Dashboard **automatically loads** your DHIS2 data!

## How to Find Your UIDs

### Data Element UIDs:
1. In DHIS2, go to: **Maintenance** → **Data Element**
2. Search for your indicator (e.g., "Suspected Malaria Cases")
3. Click on it
4. Copy the **UID** (11 characters, e.g., `UsSUX0cpKUJ`)

### Organization Unit UIDs:
1. In DHIS2, go to: **Maintenance** → **Organisation Units**
2. Click on your district/region
3. Copy the **UID** (e.g., `O6uvpzGd5pu`)
