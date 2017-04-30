* Encoding: windows-1252.
output close *.

GET FILE='c:\jason\spsswin\cdaclass\proficient.sav'.

frequencies vars=level
    /barchart.
