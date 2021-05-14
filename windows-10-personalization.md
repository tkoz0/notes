# windows 10 personalization

### dark mode

If activated, go to Settings -> Personalization -> Colors, then choose "Dark".
Otherwise, it can be edited in the registry. Go to
`Computer\HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize`
Create or edit the REG_DWORD value named "AppUseLightTheme", set it to 0.

### classic explorer

Classic Shell makes some changes to Windows Explorer. The toolbar can be
disabled on the ribbon at View -> Options, by unchecking "Classic Explorer
Bar". The status bar can be disabled in Classic Explorer settings on the "Status
Bar" tab.
