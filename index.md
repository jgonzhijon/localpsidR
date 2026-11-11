Merges Panel Study of Income Dynamics (PSID) family and individual files
into a wide-format panel data set. The user obtains the files from the
PSID data center; the package does not download data and does not handle
credentials. Files are imported once with getPSID(), which uses the
'SAScii' package to read the SAS layout files, and the panel is built
with build.panel(). It is a fork of 'psidR' by Florian Oswald.
