%MACRO nsfg_export_helper(years, name, table_name);
    PROC EXPORT
        DATA=NSFG.&name.&table_name.&years
        OUTFILE="/home/u43676684/NSFG/Export/&name.&table_name.&years..csv"
        DBMS=CSV
        REPLACE;
    RUN;
%MEND;

%MACRO nsfg_export(years, name);
    %nsfg_export_helper(&years, &name, Data);
    %nsfg_export_helper(&years, &name, Labels);
    %nsfg_export_helper(&years, &name, Formats);
%MEND;

%nsfg_export(_2017_2019, FemPreg);
%nsfg_export(_2017_2019, FemResp);
%nsfg_export(_2017_2019, Male);

%nsfg_export(_2015_2017, FemPreg);
%nsfg_export(_2015_2017, FemResp);
%nsfg_export(_2015_2017, Male);
