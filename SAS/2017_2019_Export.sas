%MACRO nsfg_export(name);

    PROC EXPORT
        DATA=NSFG.&name.Data_2017_2019
        OUTFILE="/home/u43676684/NSFG/Export/2017_2019_&name.Data.csv"
        DBMS=CSV
        REPLACE;
    RUN;
    
    PROC EXPORT
        DATA=NSFG.&name.Labels_2017_2019
        OUTFILE="/home/u43676684/NSFG/Export/2017_2019_&name.Labels.csv"
        DBMS=CSV
        REPLACE;
    RUN;
    
    PROC EXPORT
        DATA=NSFG.&name.Formats_2017_2019
        OUTFILE="/home/u43676684/NSFG/Export/2017_2019_&name.Formats.csv"
        DBMS=CSV
        REPLACE;
    RUN;

%MEND nsfg_export;

%nsfg_export(FemPreg);
%nsfg_export(FemResp);
%nsfg_export(Male);
