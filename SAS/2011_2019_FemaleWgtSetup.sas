LIBNAME NSFG '/home/u43676684/NSFG/Library';

/*--------------------------------------------------------------------------
 |
 |                   NATIONAL SURVEY OF FAMILY GROWTH (NSFG)                
 |
 |            2011-2019 MULTIPLE WEIGHT SAS FEMALE DATA SETUP FILE           
 |
 | SAS setup sections are provided for the ASCII version of this data.
 | These sections are listed below:
 |
 | DATA: begins a SAS data step and names an output SAS data set.
 | 
 | INFILE: identifies the input file to be read with the input statement.
 | Users must replace the "data-filename" with a filename specifying the 
 | directory on the user's computer system in which the downloaded and 
 | unzipped data file is physically located (e.g., "c:\temp\data.dat").
 |
 | INPUT: assigns the name, type, decimal specification (if any), and
 | specifies the beginning and ending column locations for each variable
 | in the data file.
 | 
 | LABEL: assigns descriptive labels to all variables.
 |
 |-------------------------------------------------------------------------*/

* SAS DATA, INFILE, INPUT STATEMENTS;

DATA NSFG.FemaleWgtData_2011_2019;
INFILE "/home/u43676684/NSFG/Data/2011_2019_FemaleWgtData.dat" LRECL=101;
INPUT
   CASEID  1-5              WGT2011_2015  6-21           WGT2013_2017  22-37       
   WGT2015_2019  38-53      WGT2011_2017  54-69          WGT2013_2019  70-85       
   WGT2011_2019  86-101

 ;                       

LABEL
   CASEID = "Respondent ID number"
   WGT2011_2015 = "Final 4-Year 2011-2015 Weight (Q1-Q16)"
   WGT2013_2017 = "Final 4-Year 2013-2017 Weight (Q9-Q24)"
   WGT2015_2019 = "Final 4-Year 2015-2019 Weight (Q17-Q32)"
   WGT2011_2017 = "Final 6-Year 2011-2017 Weight (Q1-Q24)"
   WGT2013_2019 = "Final 6-Year 2013-2019 Weight (Q9-Q32)"
   WGT2011_2019 = "Final 8-Year 2011-2019 Weight (Q1-Q32)" ;

RUN ;

