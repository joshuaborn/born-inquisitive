LIBNAME NSFG '/home/u43676684/NSFG/Library';

/*--------------------------------------------------------------------------
 |                                                                         
 |           NATIONAL SURVEY OF FAMILY GROWTH (NSFG), 2017-2019
 |
 |                       SAS Female Data Setup File
 |
 |
 | SAS setup sections are provided for the ASCII version of this data
 | collection.  These sections are listed below:
 |
 | PROC FORMAT:  creates user-defined formats for the variables. Formats
 | replace original value codes with value code descriptions. Only
 | variables with user-defined formats are included in this section.
 |
 | DATA:  begins a SAS data step and names an output SAS data set.
 |
 | INFILE:  identifies the input file to be read with the input statement.
 | Users must replace the "data-filename" with a filename specifying the
 | directory on the user's computer system in which the downloaded and
 | unzipped data file is physically located (e.g., "c:\temp\data.dat").
 |
 | INPUT:  assigns the name, type, decimal specification (if any), and
 | specifies the beginning and ending column locations for each variable
 | in the data file.
 |
 | LABEL:  assigns descriptive labels to all variables. Variable labels
 | and variable names may be identical for some variables.
 |
 | MISSING VALUE NOTE:  To maintain the original meaning of missing codes,
 | users may want to take advantage of the SAS missing values (the letters
 | A-Z or an underscore preceded by a period).
 |
 | An example of a standard missing value recode:
 |
 |   IF (RELATION = 98 OR RELATION = 99) THEN RELATION = .; 
 |
 | The same example using special missing value recodes:
 |
 |    IF RELATION = 98 THEN RELATION = .A;
 |    IF RELATION = 99 THEN RELATION = .B;
 |
 | FORMAT:  associates the formats created by the PROC FORMAT step with
 | the variables named in the INPUT statement.
 |
 | Users should modify this setup file to suit their specific needs.
 | To include these sections in the final SAS setup, users should remove the
 | SAS comment indicators from the desired section(s).
 |
 |-------------------------------------------------------------------------*/

* SAS PROC FORMAT;

PROC FORMAT LIBRARY=NSFG.FemRespFormats_2017_2019 CNTLOUT=NSFG.FemRespFormats_2017_2019;
   value Y1N5NALC
      1 = 'Yes'  
      5 = 'No'  
      7 = 'Not ascertained' ;
   value AGESCRN
      15 = '15 years'  
      16 = '16 years'  
      17 = '17 years'  
      18 = '18 years'  
      19 = '19 years'  
      20 = '20 years'  
      21 = '21 years'  
      22 = '22 years'  
      23 = '23 years'  
      24 = '24 years'  
      25 = '25 years'  
      26 = '26 years'  
      27 = '27 years'  
      28 = '28 years'  
      29 = '29 years'  
      30 = '30 years'  
      31 = '31 years'  
      32 = '32 years'  
      33 = '33 years'  
      34 = '34 years'  
      35 = '35 years'  
      36 = '36 years'  
      37 = '37 years'  
      38 = '38 years'  
      39 = '39 years'  
      40 = '40 years'  
      41 = '41 years'  
      42 = '42 years'  
      43 = '43 years'  
      44 = '44 years'  
      45 = '45 years'  
      46 = '46 years'  
      47 = '47 years'  
      48 = '48 years'  
      49 = '49 years'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value YESNONAF
      1 = 'Yes'  
      5 = 'No'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value RSCRRACE
      1 = 'Other race groups'  
      2 = 'Black or African American'  
      3 = 'White'  
      4 = 'Hispanic'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value AGEFMT
      15 = '15 years'  
      16 = '16 years'  
      17 = '17 years'  
      18 = '18 years'  
      19 = '19 years'  
      20 = '20 years'  
      21 = '21 years'  
      22 = '22 years'  
      23 = '23 years'  
      24 = '24 years'  
      25 = '25 years'  
      26 = '26 years'  
      27 = '27 years'  
      28 = '28 years'  
      29 = '29 years'  
      30 = '30 years'  
      31 = '31 years'  
      32 = '32 years'  
      33 = '33 years'  
      34 = '34 years'  
      35 = '35 years'  
      36 = '36 years'  
      37 = '37 years'  
      38 = '38 years'  
      39 = '39 years'  
      40 = '40 years'  
      41 = '41 years'  
      42 = '42 years'  
      43 = '43 years'  
      44 = '44 years'  
      45 = '45 years'  
      46 = '46 years'  
      47 = '47 years'  
      48 = '48 years'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value Y1N5RDF
      1 = 'Yes'  
      5 = 'No'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value HISPGRPF
      1 = 'Mexican, Mexican American, or Chicana, only'  
      2 = 'All other Hispanic or Latina groups, including multiple responses'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value PRIMLANG
      1 = 'English'  
      2 = 'Spanish'  
      7 = 'Other'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value ROSCNT
      1 = '1 HOUSEHOLD MEMBER'  
      2 = '2 HOUSEHOLD MEMBERS'  
      3 = '3 HOUSEHOLD MEMBERS'  
      4 = '4 HOUSEHOLD MEMBERS'  
      5 = '5 HOUSEHOLD MEMBERS'  
      6 = '6 HOUSEHOLD MEMBERS'  
      7 = '7 HOUSEHOLD MEMBERS'  
      8 = '8 OR MORE HOUSEHOLD MEMBERS'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value ROST4TOP
      0 = 'No children'  
      1 = '1 child'  
      2 = '2 children'  
      3 = '3 children'  
      4 = '4 children or more' ;
   value ROST5TOP
      0 = 'No children'  
      1 = '1 child'  
      2 = '2 children'  
      3 = '3 children'  
      4 = '4 children'  
      5 = '5 or more children' ;
   value DGHT918F
      0 = 'No daughters'  
      1 = '1 daughter'  
      2 = '2 or more daughters' ;
   value SON918F
      0 = '0 SONS'  
      1 = '1 SON'  
      2 = '2 OR MORE SONS' ;
   value ROST2TOP
      0 = 'No children'  
      1 = '1 child'  
      2 = '2 or more children' ;
   value MARSTAT
      1 = 'Married to a person of the opposite sex'  
      2 = 'Not married but living together with a partner of the opposite sex'  
      3 = 'Widowed'  
      4 = 'Divorced or annulled'  
      5 = 'Separated, because you and your spouse are not getting along'  
      6 = 'Never been married'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value FMARSTAT
      3 = 'Widowed'  
      4 = 'Divorced or annulled'  
      5 = 'Separated, because you and your spouse are not getting along'  
      6 = 'Never been married'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value FMARIT
      0 = 'DON''T KNOW/REFUSED'  
      1 = 'MARRIED'  
      2 = 'WIDOWED'  
      3 = 'DIVORCED'  
      4 = 'SEPARATED'  
      5 = 'NEVER MARRIED' ;
   value EVRMARRY
      0 = 'NEVER MARRIED'  
      1 = 'EVER MARRIED' ;
   value HPLOCALE
      1 = 'In household'  
      2 = 'Lives elsewhere'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value MANRELF
      1 = 'HUSBAND'  
      2 = 'MALE COHABITING PARTNER'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value HIGRADE
      9 = '9th grade or less'  
      10 = '10th grade'  
      11 = '11th grade'  
      12 = '12th grade'  
      13 = '1 year of college or less'  
      14 = '2 years of college'  
      15 = '3 years of college'  
      16 = '4 years of college/grad school'  
      17 = '5 years of college/grad school'  
      18 = '6 years of college/grad school'  
      19 = '7 or more years of college and/or grad school'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value DIPGED
      1 = 'High school diploma'  
      2 = 'GED only'  
      3 = 'Both'  
      5 = 'Neither'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value YEARFMT
      9997 = 'Not ascertained'  
      9998 = 'Refused'  
      9999 = 'Don''t know' ;
   value HISCHGRD
      1 = '1st grade'  
      2 = '2nd grade'  
      3 = '3rd grade'  
      4 = '4th grade'  
      5 = '5th grade'  
      6 = '6th grade'  
      7 = '7th grade'  
      8 = '8th grade'  
      9 = '9th grade'  
      10 = '10th grade'  
      11 = '11th grade'  
      12 = '12th grade'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value EDUCAT
      9 = '9TH GRADE OR LESS'  
      10 = '10TH GRADE'  
      11 = '11TH GRADE'  
      12 = '12TH GRADE'  
      13 = '1 YEAR OF COLLEGE/GRAD SCHOOL'  
      14 = '2 YEARS OF COLLEGE/GRAD SCHOOL'  
      15 = '3 YEARS OF COLLEGE/GRAD SCHOOL'  
      16 = '4 YEARS OF COLLEGE/GRAD SCHOOL'  
      17 = '5 YEARS OF COLLEGE/GRAD SCHOOL'  
      18 = '6 YEARS OF COLLEGE/GRAD SCHOOL'  
      19 = '7+ YEARS OF COLLEGE/GRAD SCHOOL'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value DEGREES
      1 = 'Associate''s degree'  
      2 = 'Bachelor''s degree'  
      3 = 'Master''s degree'  
      4 = 'Doctorate degree'  
      5 = 'Professional school degree'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value EXPGRADE2F
      12 = '12th grade or less'  
      13 = '1 year of college or less'  
      14 = '2 years of college'  
      15 = '3 years of college'  
      16 = '4 years of college/grad school'  
      17 = '5 years of college/grad school'  
      18 = '6 years of college/grad school'  
      19 = '7 or more years of college and/or grad school'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value WTHPARNW
      1 = 'Both biological or adoptive parents'  
      2 = 'Other or no parental figures'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value INTACT18F
      1 = 'Yes'  
      2 = 'No'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value LVSIT14F
      1 = 'Biological mother or adoptive mother'  
      2 = 'Other mother figure'  
      3 = 'No mother figure'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value LVSIT14M
      1 = 'Biological father or adoptive father'  
      2 = 'Step-father'  
      3 = 'No father figure'  
      4 = 'Other father figure'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value WOMRASDU
      1 = 'Biological mother'  
      2 = 'Other mother figure'  
      3 = 'No mother figure'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value MDDEGRE
      1 = 'Less than high school'  
      2 = 'High school graduate or GED'  
      3 = 'Some college, including 2-year degrees'  
      4 = 'Bachelor''s degree or higher'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value MOMWORKD
      1 = 'Full-time'  
      2 = 'Part-time'  
      3 = 'Equal amounts full-time and part-time'  
      4 = 'Not at all (for pay)'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value MOMFSTCH
      1 = 'LESS THAN 18 YEARS'  
      2 = '18-19 YEARS'  
      3 = '20-24 YEARS'  
      4 = '25-29 YEARS'  
      5 = '30 OR OLDER'  
      96 = 'MOTHER OR MOTHER-FIGURE DID NOT HAVE ANY CHILDREN'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value MOM18F
      1 = 'Under 18'  
      2 = '18-19'  
      3 = '20-24'  
      4 = '25 or older'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value MANRASDU
      1 = 'Biological father'  
      2 = 'Step-father'  
      3 = 'No father figure'  
      4 = 'Other father figure'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value MNYFSTER
      1 = '1 setting or location'  
      2 = '2 settings or locations'  
      3 = '3 settings or locations'  
      4 = '4 settings or locations'  
      5 = '5 or more settings or locations'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value DURFSTER
      1 = 'Less than six months'  
      2 = 'At least six months, but less than a year'  
      3 = 'At least a year but less than two years'  
      4 = 'At least two years but less than three years'  
      5 = 'Three years or more'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value AGEFSTER
      1 = 'UNDER 6 YEARS'  
      2 = '6-12 YEARS'  
      3 = '13-17 YEARS'  
      4 = '18 YEARS AND OLDER'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value MENARCHE
      8 = '8 OR YOUNGER'  
      9 = '9 YEARS'  
      10 = '10 YEARS'  
      11 = '11 YEARS'  
      12 = '12 YEARS'  
      13 = '13 YEARS'  
      14 = '14 YEARS'  
      96 = 'PERIODS HAVE NOT YET BEGUN'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value MAYBPREG
      1 = 'Probably pregnant'  
      5 = 'Probably not pregnant'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value Y1N5C
      1 = 'YES'  
      5 = 'NO' ;
   value MOSPRGF
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value KID1PLUS
      0 = 'NONE'  
      1 = '1 OR MORE CHILDREN'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value NOTHRKID
      1 = '1 CHILD'  
      2 = '2 CHILDREN'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value RELOTHKD
      1 = 'Your husband''s child (stepchild)'  
      2 = 'The child of a blood relative'  
      3 = 'The child of a relative by marriage'  
      4 = 'The child of a friend'  
      5 = 'Your boyfriend or partner''s child'  
      6 = 'Related to you in some other way'  
      7 = 'Unrelated to you previously in any way'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value ADPTOTKD
      1 = 'Yes, adopted'  
      3 = 'Yes, became guardian'  
      5 = 'No, neither'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value TRYEITHRF
      1 = 'Yes, trying to adopt'  
      3 = 'Yes, trying to become guardian'  
      5 = 'No, neither'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value NBAGECAT
      1 = 'Younger than 5 years old'  
      2 = '5-12 years old'  
      3 = '13 years or older' ;
   value EVERADPT
      1 = 'YES, HAS ADOPTED A CHILD'  
      3 = 'NO, BUT IS CURRENTLY TRYING TO ADOPT SPECIFIC CHILD'  
      5 = 'NO, AND NOT TRYING TO ADOPT A SPECIFIC CHILD' ;
   value TRYLONG
      1 = 'Less than 1 year'  
      2 = '1-2 years'  
      3 = 'Or longer than 2 years'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value TURNDOWN
      1 = 'Turned down'  
      2 = 'Unable to find child'  
      3 = 'Decided not to pursue'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value YQUITTRY
      1 = 'Adoption process only'  
      2 = 'Own situation only'  
      3 = 'Both'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value APROCESSF
      1 = 'Fees were too high'  
      2 = 'There were not enough kids available'  
      3 = 'Some other reason'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value SRCEMBRYO
      1 = 'Health professional or counselor'  
      2 = 'Relative or friend'  
      3 = 'Television, radio or a magazine'  
      4 = 'Internet'  
      5 = 'Other'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value TIMESMAR
      1 = '1 TIME'  
      2 = '2 TIMES'  
      3 = '3 TIMES'  
      4 = '4 TIMES'  
      5 = '5 TIMES'  
      6 = '6 TIMES'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value AGEHP
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value ENGAGF
      1 = 'YES, ENGAGED TO BE MARRIED'  
      3 = 'NOT ENGAGED BUT HAD DEFINITE PLANS TO GET MARRIED'  
      5 = 'NO, NEITHER ENGAGED NOR HAD DEFINITE PLANS'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value RACEFMTD
      1 = 'Black'  
      2 = 'White'  
      3 = 'Other'  
      4 = 'NA/DK/RF' ;
   value NRACED
      1 = 'Hispanic'  
      2 = 'Non-Hispanic White, Single Race'  
      3 = 'Non-Hispanic Black, Single Race'  
      4 = 'Non-Hispanic Other or Multiple Race'  
      5 = 'NA/DK/RF' ;
   value EDUCFMT
      1 = 'Less than high school'  
      2 = 'High school graduate or GED'  
      3 = 'Some college but no degree'  
      4 = '2-year college degree (e.g., Associate''s degree)'  
      5 = '4-year college graduate (e.g., BA, BS)'  
      6 = 'Graduate or professional school'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value NUMKDS
      1 = '1 CHILD'  
      2 = '2 OR MORE CHILDREN'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value KID18BF
      0 = 'NONE'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value WHRKDSF
      1 = 'In this household'  
      2 = 'With their mother'  
      3 = 'With grandparents or other relatives'  
      4 = 'Somewhere else'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value Y1N5JPC
      1 = 'Yes'  
      5 = 'No'  
      6 = 'IF VOLUNTEERED: JOINT PHYSICAL CUSTODY'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value BIONUMFF
      1 = '1 CHILD'  
      2 = '2 CHILDREN'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value PREVHUSB
      0 = 'NONE'  
      1 = '1 FORMER HUSBAND'  
      2 = '2 FORMER HUSBANDS'  
      3 = '3 FORMER HUSBANDS'  
      4 = '4 FORMER HUSBANDS'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value AGERFEMC
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value DEFPROBF
      1 = 'Definitely yes'  
      2 = 'Probably yes'  
      3 = 'Probably no'  
      4 = 'Definitely no'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value HMOTHMEN
      1 = '1 MAN'  
      2 = '2 MEN'  
      3 = '3 MEN'  
      4 = '4 OR MORE MEN'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value PREVCOHB
      0 = 'NONE'  
      1 = '1 FORMER COHABITING PARTNER'  
      2 = '2 FORMER COHABITING PARTNERS'  
      3 = '3 FORMER COHABITING PARTNERS'  
      4 = '4 OR MORE COHABITING PARTNERS'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value AGEHPNA
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value YNOSEX
      1 = 'Against religion or morals'  
      2 = 'Don''t want to get pregnant'  
      3 = 'Don''t want to get a sexually transmitted disease'  
      4 = 'Haven''t found the right person yet'  
      5 = 'In a relationship, but waiting for the right time'  
      6 = 'Other'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value GRFSTSXF
      6 = '6th grade or less'  
      7 = '7th grade'  
      8 = '8th grade'  
      9 = '9th grade'  
      10 = '10th grade'  
      11 = '11th grade'  
      12 = '12th grade'  
      13 = '1st year in college'  
      14 = '2nd year in college'  
      15 = '3rd year in college'  
      16 = '4th year in college'  
      96 = 'Not in school'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value TALKPARF
      1 = 'How to say no to sex'  
      2 = 'Methods of birth control'  
      3 = 'Where to get birth control'  
      4 = 'Sexually transmitted diseases'  
      5 = 'How to prevent HIV/AIDS'  
      6 = 'How to use a condom'  
      8 = 'Waiting until marriage to have sex'  
      95 = 'None of the above'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value SEDFMT
      1 = 'School'  
      2 = 'Church'  
      3 = 'A community center'  
      4 = 'Some other place'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value SXEDGRF
      1 = '1st grade'  
      2 = '2nd grade'  
      3 = '3rd grade'  
      4 = '4th grade'  
      5 = '5th grade'  
      6 = '6th grade'  
      7 = '7th grade'  
      8 = '8th grade'  
      9 = '9th grade'  
      10 = '10th grade'  
      11 = '11th grade'  
      12 = '12th grade'  
      13 = '1st year of college'  
      14 = '2nd year of college'  
      15 = '3rd year of college'  
      16 = '4th year of college'  
      96 = 'Not in school when received instruction'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value BFAFTF
      1 = 'Before'  
      2 = 'After'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value SXEDWHRINS
      1 = 'School'  
      2 = 'Church'  
      3 = 'A community center'  
      4 = 'Some other place'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value MTCHMANF
      1 = 'Your first former husband, [NAME], married [DATE], marriage ended [DATE]'  
      2 = 'Your second former husband, [NAME], married [DATE], marriage ended [DATE]'  
      3 = 'Your third former husband, [NAME], married [DATE], marriage ended [DATE]'  
      4 = 'Your fourth former husband, [NAME], married [DATE], marriage ended [DATE]'  
      5 = 'Your fifth former husband, [NAME], married [DATE], marriage ended [DATE]'  
      6 = 'Your sixth former husband, [NAME], married [DATE], marriage ended [DATE]'  
      7 = 'Your current husband, [NAME], married [DATE], still married or (Your husband, from whom you are separated, married [DATE], still married'  
      8 = 'Your current partner, [NAME], began living together [DATE], still living together'  
      9 = 'Your first former partner [NAME], began living together [DATE], stopped living together [DATE]'  
      10 = 'Your second cohabiting partner, [NAME], began living together [DATE], stopped living together [DATE]'  
      11 = 'Your third cohabiting partner, [NAME], began living together [DATE], stopped living together [DATE]'  
      12 = 'Your fourth cohabiting partner, [NAME], began living together [DATE], stopped living together [DATE]'  
      20 = 'None of the Above'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value FSEXREL
      1 = 'Married to him'  
      2 = 'Engaged to him'  
      3 = 'Living together in a sexual relationship, but not engaged'  
      4 = 'Going with him or going steady'  
      5 = 'Going out with him once in a while'  
      6 = 'Just friends'  
      7 = 'Had just met him'  
      8 = 'Something else'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value LSTSEXFPF
      13 = 'Winter'  
      14 = 'Spring'  
      15 = 'Summer'  
      16 = 'Fall'  
      96 = 'Only had sex once'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value CMLSEXFP
      9996 = 'Only had sex once with first partner'  
      9998 = 'Refused'  
      9999 = 'Don''t know' ;
   value CMFMT
      9997 = 'Not ascertained'  
      9998 = 'Refused'  
      9999 = 'Don''t know' ;
   value CURRREL
      2 = 'Engaged to him'  
      4 = 'Going with him or going steady'  
      5 = 'Going out with him once in a while'  
      6 = 'Just friends'  
      7 = 'Had just met him'  
      8 = 'Something else'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value WHICH1ST
      1 = 'Sexual intercourse'  
      2 = 'Menstrual period'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value PTSB4MAR
      0 = 'NONE'  
      1 = '1 PARTNER'  
      2 = '2 PARTNERS'  
      3 = '3 PARTNERS'  
      4 = '4 PARTNERS'  
      5 = '5 PARTNERS'  
      6 = '6 PARTNERS'  
      7 = '7 PARTNERS'  
      8 = '8 PARTNERS'  
      9 = '9 PARTNERS'  
      20 = '20 OR MORE PARTNERS'  
      998 = 'REFUSED'  
      999 = 'DON''T KNOW' ;
   value P1YRELP
      1 = 'First former husband'  
      2 = 'Second former husband'  
      3 = 'Third former husband'  
      4 = 'Fourth former husband'  
      5 = 'Fifth former husband'  
      6 = 'Sixth former husband'  
      7 = 'Current husband or (current husband from whom she is separated)'  
      8 = 'Current cohabiting partner'  
      9 = 'First former cohabiting partner'  
      10 = 'Second former cohabiting partner'  
      11 = 'Third former cohabiting partner'  
      12 = 'Fourth former cohabiting partner'  
      13 = 'First sexual partner'  
      20 = 'None of the above'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value MNTHFMT
      13 = 'Winter'  
      14 = 'Spring'  
      15 = 'Summer'  
      16 = 'Fall'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value P1YRAGE
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value P1YHSAGE
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value P1YFSEXF
      13 = 'Winter'  
      14 = 'Spring'  
      15 = 'Summer'  
      16 = 'Fall'  
      96 = 'Only had sex once with this partner'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value CMFSEX
      9996 = 'Only had sex once with this partner'  
      9998 = 'Refused'  
      9999 = 'Don''t know' ;
   value CURRPRTS
      0 = 'NONE'  
      1 = '1 PARTNER' ;
   value EVERTUBS
      1 = 'Yes'  
      3 = 'If volunteered: Operation failed'  
      4 = 'If volunteered: Had Essure procedure'  
      5 = 'No'  
      6 = 'If volunteered: Operation already reversed'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value WHTOOPRC
      1 = 'Operation affects only one tube'  
      2 = 'Operation affects only one ovary'  
      3 = 'Some other operation'  
      4 = 'Other sterilizing operation'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value WHATOPSM
      1 = 'Vasectomy'  
      2 = 'Other operation'  
      5 = 'If volunteered: Operation failed'  
      6 = 'If volunteered: Operation already reversed'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value Y1N2RECF
      1 = 'YES'  
      2 = 'NO' ;
   value PLCOPF
      1 = 'Private doctor''s office'  
      2 = 'HMO facility'  
      3 = 'Community health clinic, community clinic, or public health clinic'  
      4 = 'Family planning or Planned Parenthood clinic'  
      5 = 'Employer or company clinic'  
      6 = 'School or school-based'  
      7 = 'Hospital outpatient clinic'  
      8 = 'Hospital emergency room'  
      9 = 'Hospital regular room'  
      10 = 'Urgent care center, urgi-care or walk-in facility'  
      11 = 'In-store health clinic (like CVS, Target, or Walmart)'  
      20 = 'Some other place'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value PAYFMT2F
      1 = 'Insurance'  
      2 = 'Co-payment or out-of-pocket payment'  
      3 = 'Medicaid'  
      4 = 'No payment required'  
      5 = 'Some other way'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value HHADALL
      1 = 'Yes'  
      5 = 'No'  
      6 = 'If volunteered: R was not in a relationship with a man at the time she had this operation'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value FMEDREASF
      1 = 'Medical problems with your female organs'  
      2 = 'Pregnancy would be dangerous to your health'  
      3 = 'You would probably lose a pregnancy'  
      4 = 'You would probably have an unhealthy child'  
      5 = 'Some other medical reason'  
      6 = 'No medical reason for operation'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value BCREAS
      1 = 'Yes'  
      5 = 'No'  
      6 = 'No, not using any method at the time'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value BCWHYFF
      1 = 'Health or medical problem'  
      2 = 'Some other reason'  
      3 = 'Both'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value MINCDNF
      1 = 'You had all the children you wanted'  
      2 = 'Your husband or partner had all the children he wanted'  
      3 = 'Medical reasons'  
      4 = 'Problems with other methods of birth control'  
      5 = 'Some other reason not mentioned above'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value MEDREASF
      1 = 'Pregnancy would be dangerous to your health'  
      2 = 'You would probably lose a pregnancy'  
      3 = 'You would probably have an unhealthy child'  
      4 = 'He had health problem that required the operation'  
      5 = 'Some other medical reason'  
      6 = 'No medical reason for operation'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value DFPRBNAF
      1 = 'Definitely yes'  
      2 = 'Probably yes'  
      3 = 'Probably no'  
      4 = 'Definitely no'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value REASR
      1 = 'Impossible due to problems with ovulation'  
      2 = 'Impossible due to problems with uterus, cervix, or fallopian tubes'  
      3 = 'Impossible due to other illnesses or treatment for other illnesses such as cancer'  
      4 = 'Impossible due to menopause'  
      20 = 'Impossible for other reasons'  
      30 = 'R volunteers it is not impossible for her'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value REASP
      1 = 'Impossible due to problems with sperm or semen'  
      2 = 'Impossible due to testicular problems or varicocele'  
      3 = 'Impossible due to other illnesses or treatment for other illnesses'  
      4 = 'Impossible for other reasons'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value REASDIFFF
      1 = 'You have difficulty getting pregnant'  
      2 = 'You have difficulty carrying baby to term'  
      3 = 'Pregnancy is dangerous to your health'  
      4 = 'You are likely to have an unhealthy baby'  
      5 = 'Some other reason'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value REASNONOF
      1 = 'Dangerous for you'  
      2 = 'Dangerous for your baby'  
      3 = 'Some other reason'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value RSTRSTAT
      0 = 'NOT STERILE'  
      1 = 'SURGICALLY STERILE'  
      2 = 'NONSURGICALLY STERILE'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value LASTPERF
      1 = 'Within the past 4 weeks'  
      2 = 'Longer ago than 4 weeks, but less than 3 months'  
      3 = 'Longer ago than 3 months, but less than 6 months'  
      4 = 'Longer ago than 6 months, but less than 1 year'  
      5 = 'Longer ago than 1 year'  
      95 = 'If volunteered: Before last birth or pregnancy'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value ONETWO2F
      1 = '1'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value ECREASONF
      1 = 'You were worried your birth control method would not work'  
      2 = 'You didn''t use birth control that time'  
      3 = 'Some other reason'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value ECRX
      1 = 'With a prescription'  
      2 = 'Without a prescription'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value PLACE1FMT
      1 = 'Private doctor''s office'  
      2 = 'HMO facility'  
      3 = 'Community health clinic, community clinic, public health clinic'  
      4 = 'Family planning or Planned Parenthood clinic'  
      5 = 'Employer or company clinic'  
      6 = 'School or school-based clinic'  
      7 = 'Hospital outpatient clinic'  
      8 = 'Hospital emergency room'  
      9 = 'Hospital regular room'  
      10 = 'Urgent care center, urgi-care, or walk-in facility'  
      11 = 'Friend'  
      12 = 'Partner or spouse'  
      13 = 'Drug store'  
      14 = 'Mail order/Internet'  
      15 = 'In-store health clinic (like CVS, Target, or Walmart)'  
      20 = 'Some other place'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value ECWHEN
      1 = 'Yes (Within the last 12 months)'  
      2 = 'No (Over 12 months ago)'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value OTHRMETHF
      9 = 'Hormonal implant (Norplant, Implanon, or Nexplanon)'  
      12 = 'Diaphragm'  
      13 = 'Female condom, vaginal pouch'  
      14 = 'Foam'  
      15 = 'Jelly or cream'  
      16 = 'Cervical cap'  
      17 = 'Suppository, insert'  
      18 = 'Today sponge'  
      19 = 'IUD'  
      21 = 'Other method -- specify'  
      24 = 'Lunelle'  
      95 = 'No other methods ever used'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value IUDTYPE
      1 = 'Copper-bearing (such as Copper-T or ParaGard)'  
      2 = 'Hormonal IUD (such as Mirena, Skyla, Liletta, or Kyleena)'  
      3 = 'Other'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value NEWMETHF
      1 = 'Abstinence'  
      2 = 'Tubal ligation/female sterilization'  
      3 = 'Vaginal contraceptive film'  
      4 = 'Other method, not shown separately'  
      5 = 'Response was coded as a method in EA-1 through EA-14'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value METHSTOPF
      3 = 'Birth control pills'  
      4 = 'Condom'  
      5 = 'Partner''s vasectomy'  
      6 = 'Female sterilizing operation, such as tubal sterilization and hysterectomy'  
      7 = 'Withdrawal, pulling out'  
      8 = 'Depo-Provera, injectables (shots)'  
      9 = 'Hormonal implant (Norplant, Implanon, or Nexplanon)'  
      10 = 'Calendar rhythm, Standard Days, or Cycle Beads method'  
      11 = 'Safe period by temperature or cervical mucus test (Two Day, Billings Ovulation, or Sympto-thermal Method)'  
      12 = 'Diaphragm'  
      13 = 'Female condom, vaginal pouch'  
      14 = 'Foam'  
      15 = 'Jelly or cream'  
      16 = 'Cervical cap'  
      17 = 'Suppository, insert'  
      18 = 'Today sponge'  
      19 = 'IUD'  
      21 = 'Other method'  
      24 = 'Lunelle injectable (monthly shot)'  
      25 = 'Contraceptive patch (Ortho-Evra or Xulane)'  
      26 = 'Vaginal contraceptive ring'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value WHENMTHF
      1 = 'Yes (stopped within the last 12 months)'  
      5 = 'No (stopped over 12 months ago)'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value REASMFMT
      1 = 'Too expensive'  
      2 = 'Insurance did not cover it'  
      3 = 'Too difficult to use'  
      4 = 'Too messy'  
      5 = 'Your partner did not like it'  
      6 = 'You had side effects'  
      7 = 'You were worried you might have side effects'  
      8 = 'You worried the method would not work'  
      9 = 'The method failed, you became pregnant'  
      10 = 'The method did not protect against disease'  
      11 = 'Because of other health problems, a doctor told you that you should not use the method again'  
      12 = 'The method decreased your sexual pleasure'  
      13 = 'Too difficult to obtain the method'  
      14 = 'Did not like the changes to your menstrual cycle'  
      15 = 'Other -- specify'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value STOPPILLFMT
      1 = 'Can''t remember to take pill (regularly)'  
      2 = 'Interfered with other medications'  
      3 = 'Weight gain'  
      4 = 'Headaches/migraines'  
      5 = 'Mood swings/depression'  
      6 = 'Bleeding problems (spotting, too much, irregularly)'  
      7 = 'Hair loss'  
      8 = 'Nausea/sickness'  
      9 = 'Skin problems'  
      10 = 'Hormonal problems/changes'  
      11 = 'Didn''t like/Didn''t want to use'  
      12 = 'Concerns about risks, such as breast cancer'  
      13 = 'Not effective at preventing cramps'  
      14 = 'Weight loss'  
      15 = 'Menstrual cramps'  
      16 = 'Leg cramps'  
      17 = 'Dizziness'  
      18 = 'Decreased sex drive'  
      19 = 'High blood pressure'  
      20 = 'Blood clots'  
      21 = 'Fatigue'  
      22 = 'Other side effects'  
      23 = 'Other (Too few cases, not classifiable elsewhere)'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value STOPCONDFMT
      1 = 'Allergy (latex/spermicide)'  
      2 = 'Didn''t like it / Didn''t like how it felt'  
      3 = 'Pain/discomfort/irritation'  
      4 = 'Yeast infection'  
      5 = 'Worried it would fall off / break / not prevent pregnancy'  
      6 = 'Inconvenient'  
      7 = 'Not spontaneous'  
      8 = 'Frustrating to use'  
      9 = 'Other (Too few cases, not classifiable elsewhere)'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value STOPIUDFMT
      1 = 'Weight gain'  
      2 = 'Bleeding problems (spotting, too much, irregularly)'  
      3 = 'Headaches/migraines'  
      4 = 'Mood swings/depression'  
      5 = 'Hair loss or growth'  
      6 = 'Vaginal infection'  
      7 = 'Abdominal pain or cramping; back pain'  
      8 = 'Skin problems'  
      9 = 'Pain or discomfort during intercourse'  
      10 = 'Nausea/sickness'  
      11 = 'IUD moved or was expelled'  
      12 = 'Other side effects'  
      13 = 'Other (Too few cases, not classifiable elsewhere)'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value REMOVWHYF
      1 = 'I wanted to get pregnant'  
      2 = 'I didn''t like the side effects'  
      3 = 'The method caused complications (for example, moved inside your body)'  
      4 = 'It was expired and I needed a new one'  
      5 = 'Other'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value REMVDIFY
      1 = 'Provider discouraged me from getting implant or IUD removed'  
      2 = 'Provider didn''t know how to remove implant or IUD'  
      3 = 'Insurance didn''t cover removal of implant or IUD'  
      4 = 'Complications because of the method (for example: moved, became lodged)'  
      5 = 'I still have my implant or IUD because I can''t get it removed'  
      6 = 'Other'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value FIRSMETHF
      3 = 'Birth control pills'  
      4 = 'Condom'  
      5 = 'Partner''s vasectomy'  
      6 = 'Female sterilizing operation, such as tubal sterilization and hysterectomy'  
      7 = 'Withdrawal, pulling out'  
      8 = 'Depo-Provera, injectables'  
      9 = 'Hormonal implant (Norplant, Implanon, or Nexplanon)'  
      10 = 'Calendar rhythm, Standard Days, or Cycle Beads method'  
      11 = 'Safe period by temperature or cervical mucus test (Two Day, Billings Ovulation, or Sympto-thermal Method)'  
      12 = 'Diaphragm'  
      13 = 'Female condom, vaginal pouch'  
      14 = 'Foam'  
      15 = 'Jelly or cream'  
      16 = 'Cervical cap'  
      17 = 'Suppository, insert'  
      18 = 'Today sponge'  
      19 = 'IUD'  
      20 = 'Emergency contraception'  
      21 = 'Other method -- specify'  
      22 = 'Respondent was sterile'  
      23 = 'Respondent''s partner was sterile'  
      24 = 'Lunelle injectable (monthly shot)'  
      25 = 'Contraceptive patch (Ortho-Evra or Xulane)'  
      26 = 'Vaginal contraceptive ring'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value ONETWOF
      0 = '0'  
      1 = '1'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value Y1N52D
      1 = 'Yes'  
      5 = 'No'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value FIRSTIME1F
      2 = 'The first time you had intercourse'  
      3 = 'Less than a month after your first intercourse'  
      4 = 'One to three months after first intercourse'  
      5 = 'Four to twelve months after first intercourse'  
      6 = 'More than twelve months after first intercourse'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value FIRSTIME2F
      1 = 'Before your first intercourse'  
      2 = 'The first time you had intercourse'  
      3 = 'Less than a month after your first intercourse'  
      4 = 'One to three months after first intercourse'  
      5 = 'Four to twelve months after first intercourse'  
      6 = 'More than twelve months after first intercourse'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value USEFSTSX
      1 = 'YES'  
      5 = 'NO'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value METHPRSF
      0 = '1st meth was nonprescription'  
      1 = '1st meth was prescription' ;
   value AGE44NRDF
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value MCMONS
      995 = 'R reported a month and year instead'  
      998 = 'Refused'  
      999 = 'Don''t know' ;
   value MC1SIMSQ
      1 = 'Same time'  
      2 = 'Different times'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value METHHXF
      1 = 'No method used'  
      2 = 'EMPTY (IF JAN)/Same as previous month'  
      3 = 'Birth control pills'  
      4 = 'Condom'  
      5 = 'Partner''s vasectomy'  
      6 = 'Female sterilizing operation, such as tubal sterilization and hysterectomy'  
      7 = 'Withdrawal, pulling out'  
      8 = 'Depo-Provera, injectables'  
      9 = 'Hormonal implant (Norplant, Implanon, or Nexplanon)'  
      10 = 'Calendar rhythm, Standard Days, or Cycle Beads method'  
      11 = 'Safe period by temperature or cervical mucus test (Two Day, Billings Ovulation, or Sympto-thermal Method)'  
      12 = 'Diaphragm'  
      13 = 'Female condom, vaginal pouch'  
      14 = 'Foam'  
      15 = 'Jelly or cream'  
      16 = 'Cervical cap'  
      17 = 'Suppository, insert'  
      18 = 'Today sponge'  
      19 = 'IUD'  
      20 = 'Emergency contraception'  
      21 = 'Other method'  
      22 = 'Respondent was sterile'  
      23 = 'Respondent''s partner was sterile'  
      25 = 'Contraceptive patch (Ortho-Evra or Xulane)'  
      26 = 'Vaginal contraceptive ring'  
      55 = 'EMPTY (IF JAN)/Same method used thru end of year'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value METHYNF
      1 = 'Used in past 12 months'  
      5 = 'Not used in past 12 mos or dk/rf'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value NUMMULTF
      0 = '0'  
      1 = '1'  
      2 = '2'  
      3 = '3'  
      4 = '4'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value N0Y1F
      0 = 'No'  
      1 = 'Yes' ;
   value HPPREGQ
      1 = 'Yes'  
      5 = 'No'  
      6 = '(If volunteered:) no current partner'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value DURTRY_N
      95 = 'Not trying to get pregnant'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value MNTHYRRD
      1 = 'Months'  
      2 = 'Years'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value WHYNOUSNG
      1 = 'You do not expect to have sex'  
      2 = 'You do not think you can get pregnant'  
      3 = 'You don''t really mind if you get pregnant'  
      4 = 'You are worried about the side effects of birth control'  
      5 = 'Your male partner does not want you to use a birth control method'  
      6 = 'Your male partner himself does not want to use a birth control method'  
      7 = '(IF VOLUNTEERED:)Respondent is using a method'  
      8 = 'You could not get a method'  
      9 = 'You are not taking, or using, your method consistently'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value WHYNOTPGF
      1 = 'Diagnosis of repro problems/condition'  
      2 = 'Prior preg or attempt: problems with or treatments/ART needed'  
      3 = 'Prior unprotected sex, no preg'  
      4 = 'Perceive infertile or difficulty getting preg/carrying to term'  
      5 = 'Menopause related'  
      6 = 'One ovary/fallopian tube/low or no ovulation'  
      7 = 'Endometriosis'  
      8 = 'Breastfeeding'  
      9 = 'Partner sterile'  
      10 = 'Not having sex'  
      11 = 'Other'  
      12 = 'Too old (R or partner)'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value MAINNOUSE
      1 = 'You do not expect to have sex'  
      2 = 'You do not think you can get pregnant'  
      3 = 'You don''t really mind if you get pregnant'  
      4 = 'You are worried about the side effects of birth control'  
      5 = 'Your male partner does not want you to use a birth control method'  
      6 = 'Your male partner himself does not want to use a birth control method'  
      7 = '(IF VOLUNTEERED:)Respondent is using a method'  
      8 = 'You could not get a method'  
      9 = 'You are not taking, or using, your method consistently'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value YUSEMTHF
      1 = 'Birth control'  
      2 = 'Cramps, or pain during menstrual periods'  
      3 = 'Treatment for acne'  
      4 = 'Treatment for endometriosis'  
      5 = 'Other reasons'  
      6 = 'To regulate your menstrual periods'  
      7 = 'To reduce menstrual bleeding'  
      8 = 'Treatment for hot flashes or other peri-menopausal symptoms'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value BCPLCF
      1 = 'Private doctor''s office'  
      2 = 'HMO facility'  
      3 = 'Community health clinic, community clinic, public health clinic'  
      4 = 'Family planning or Planned Parenthood Clinic'  
      5 = 'Employer or company clinic'  
      6 = 'School or school-based clinic'  
      7 = 'Hospital outpatient clinic'  
      8 = 'Hospital emergency room'  
      9 = 'Hospital regular room'  
      10 = 'Urgent care center, urgi-care or walk-in facility'  
      11 = 'In-store health clinic (like CVS, Target, or Walmart)'  
      20 = 'Some other place'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value NUMPILLS
      1 = '1 month'  
      2 = '2 months'  
      3 = '3 months'  
      12 = '12 months'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value PAYFMT
      1 = 'Insurance'  
      2 = 'Co-payment'  
      3 = 'Out-of-pocket payment'  
      4 = 'Medicaid'  
      5 = 'No payment required'  
      6 = 'Some other way'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value NOUSEINSF
      1 = 'Insurance doesn''t cover my method supplies'  
      2 = 'I had not yet met my insurance deductible'  
      3 = 'I did not want to use insurance because someone might find out about it'  
      4 = 'I did not need to use insurance because the method supplies were free'  
      5 = 'Some other reason'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value CURBCAMT
      1 = 'Under $10'  
      2 = '$10-$25'  
      3 = '$26-$50'  
      4 = '$51-$100'  
      5 = 'Over $100'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value PSTWKSF
      998 = 'Refused'  
      999 = 'Don''t know' ;
   value CONDTMS
      0 = 'Not at all or never'  
      998 = 'Refused'  
      999 = 'Don''t know' ;
   value MISSPILL
      1 = 'Never missed'  
      2 = 'Missed only one'  
      3 = 'Missed two or more'  
      4 = 'Did not use pill over past 4 weeks'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value P12MOCON
      1 = 'Every time'  
      2 = 'Most of the time'  
      3 = 'About half of the time'  
      4 = 'Some of the time'  
      5 = 'None of the time'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value P12METHF
      1 = 'Every time'  
      2 = 'Most of the time'  
      3 = 'About half of the time'  
      4 = 'Some of the time'  
      5 = 'None of the time'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value NUMMTH12A
      0 = 'No methods'  
      1 = '1 method'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value BARRIERF
      1 = 'I did not need to see a doctor in the last year'  
      2 = 'I did not know where to go for care'  
      3 = 'I could not afford to pay for a visit'  
      4 = 'I was afraid to hear bad news'  
      5 = 'I had privacy/confidentiality concerns'  
      6 = 'I could not take time off from work'  
      7 = 'Insurance'  
      8 = 'Not sexually active'  
      9 = 'Time/busy'  
      10 = 'Didn''t make appt'  
      11 = 'Don''t like/trust doctors'  
      20 = 'Something else (please specify)'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value NUMSVC12A
      0 = 'No services'  
      1 = '1 service'  
      2 = '2 services'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value NUMBCVIS
      1 = 'Single visit'  
      5 = 'More than one visit'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value IDCLINIC
      0 = '0'  
      1 = '1'  
      2 = '2'  
      3 = '3'  
      4 = '4'  
      5 = '5'  
      6 = '6'  
      7 = '7'  
      8 = '8'  
      9 = '9'  
      10 = '10'  
      11 = '11'  
      12 = '12' ;
   value TALKPROVF
      1 = 'Birth control methods (including IUD and implants)'  
      2 = 'Condoms for STD prevention'  
      3 = 'HPV vaccine'  
      4 = 'None of the above'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value WHYPSTDF
      1 = 'Could walk in or get same-day appointment'  
      2 = 'Cost'  
      3 = 'Privacy concern'  
      4 = 'Expert care here'  
      5 = 'Embarrassed to go to usual provider'  
      6 = 'Other'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value WHYNOSTDF
      1 = 'Didn''t want parents to find out'  
      2 = 'Concerned about confidentiality'  
      3 = 'Doctor or health care provider never suggested it'  
      4 = 'Embarrassed or difficult to ask to be tested'  
      5 = 'Cost or lack of insurance'  
      6 = 'Other' ;
   value REGCAR12F
      1 = 'Regular place'  
      2 = 'Regular place, but go to more than 1 place regularly'  
      3 = 'Usually go somewhere else'  
      4 = 'No usual place'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value KNDMDHLPF
      1 = 'A method of birth control (or prescription)'  
      2 = 'Birth control counseling'  
      3 = 'Emergency contraception'  
      4 = 'Counseling about Emergency Contraception'  
      5 = 'A check-up or test for birth control'  
      6 = 'A pregnancy test'  
      7 = 'An abortion'  
      8 = 'A Pap smear or pelvic exam'  
      9 = 'Post-pregnancy care'  
      10 = 'STD or HIV testing'  
      20 = 'Other'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value LASTPAP
      1 = 'A year ago or less'  
      2 = 'More than 1 year ago but not more than 2 years'  
      3 = 'More than 2 years ago but not more than 3 years'  
      4 = 'More than 3 years ago but not more than 5 years'  
      5 = 'Over 5 years ago'  
      6 = 'Never had Pap test'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value MREASFMT
      1 = 'Part of a routine exam'  
      2 = 'Because of a medical problem'  
      3 = 'Other reason'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value AGEFIRST
      12 = '12 years or younger'  
      13 = '13 years'  
      14 = '14 years'  
      15 = '15 years'  
      16 = '16 years'  
      17 = '17 years'  
      18 = '18 years'  
      19 = '19 years'  
      20 = '20 years'  
      21 = '21 years'  
      22 = '22 years'  
      23 = '23 years'  
      24 = '24 years'  
      25 = '25 years'  
      26 = '26 years'  
      27 = '27 years'  
      28 = '28 years'  
      29 = '29 years'  
      30 = '30 years or older' ;
   value AGEF2F
      1 = 'Younger than 18'  
      2 = '18-21'  
      3 = '22-29'  
      4 = '30 or older'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value ABNPAP3F
      1 = 'Yes'  
      5 = 'No'  
      6 = 'No Pap test in past 3 years'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value INTOFTF
      1 = 'Every year'  
      2 = 'Every 2 years'  
      3 = 'Every 3 years'  
      4 = 'Every 4 years'  
      7 = 'Every 5 years'  
      8 = 'Less often than every 5 years'  
      95 = 'If R volunteers: More than once a year'  
      96 = 'If R volunteers: She would never need to be tested'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value LASTPEL
      1 = 'A year ago or less'  
      2 = 'More than 1 year ago but not more than 2 years'  
      3 = 'More than 2 years ago but not more than 3 years'  
      4 = 'More than 3 years ago but not more than 5 years'  
      5 = 'Over 5 years ago'  
      6 = 'Never had pelvic exam'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value LASTHPV
      1 = 'A year ago or less'  
      2 = 'More than 1 year ago but not more than 2 years'  
      3 = 'More than 2 years ago but not more than 3 years'  
      4 = 'More than 3 years ago but not more than 5 years'  
      5 = 'Over 5 years ago'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value PROVRATE
      1 = 'Poor'  
      2 = 'Fair'  
      3 = 'Good'  
      4 = 'Very good'  
      5 = 'Excellent'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value PROBWANT
      1 = 'Probably want'  
      2 = 'Probably do not want'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value SUREINTCHP
      1 = 'Very sure'  
      2 = 'Somewhat sure'  
      3 = 'Not at all sure'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value INTEXPF
      0 = 'NONE'  
      1 = '1 CHILD'  
      2 = '2 CHILDREN'  
      3 = '3 CHILDREN'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value JEXPECTS
      0 = 'NONE'  
      1 = '1 CHILD'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value INTNEXT
      1 = 'Within the next 2 years'  
      2 = '2-5 years from now'  
      3 = 'More than 5 years from now'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value HOWMANYR
      1 = 'One'  
      2 = 'More than one'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value SEEKWHO1F
      1 = 'Current husband/partner'  
      5 = 'Another partner'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value TYPALLPGF
      1 = 'Advice'  
      2 = 'Infertility testing'  
      3 = 'Drugs to improve your ovulation'  
      4 = 'Surgery to correct blocked tubes'  
      5 = 'Artificial insemination'  
      6 = 'Other types of medical help'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value WHOTEST
      1 = 'You'  
      3 = 'Him'  
      5 = 'Both of you'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value WHARTIN
      1 = 'Husband or partner'  
      3 = 'Donor'  
      5 = 'Both husband or partner and donor'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value OTMEDHEPF
      1 = 'Surgery or drug treatment for endometriosis'  
      2 = 'In vitro fertilization (IVF)'  
      3 = 'Surgery or drug treatment for uterine fibroids'  
      4 = 'Some other female pelvic surgery'  
      5 = 'Other medical help'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value TRYLONG2F
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value UNIT_TRYLONG
      1 = 'Months'  
      5 = 'Years'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value RCNTPGHF
      13 = 'Winter'  
      14 = 'Spring'  
      15 = 'Summer'  
      16 = 'Fall'  
      96 = 'R only had one visit for help to become pregnant'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value CMPGVISL
      9996 = 'Only had 1 visit'  
      9998 = 'Refused'  
      9999 = 'Don''t know' ;
   value NUMVSTPG
      1 = '1 VISIT'  
      2 = '2 VISITS'  
      3 = '3 VISITS'  
      4 = '4 VISITS'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value PRGVISIT
      0 = 'NO VISITS IN LAST 12 MONTHS'  
      1 = '1 VISIT'  
      2 = '2 VISITS'  
      3 = '3 VISITS'  
      4 = '4 VISITS'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value TYPALLMCF
      1 = 'Instructions to take complete bed rest'  
      2 = 'Instructions to limit your physical activity'  
      3 = 'Testing to diagnose problems related to miscarriage'  
      4 = 'Drugs to prevent miscarriage, such as progesterone suppositories'  
      5 = 'Stitches in your cervix, also known as the ''purse-string'' procedure'  
      6 = 'Other types of medical help'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value TPRGMISS
      0 = 'NONE'  
      1 = '1 PREGNANCY'  
      2 = '2 PREGNANCIES'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value INFRTPRBF
      1 = 'Problems with ovulation'  
      2 = 'Blocked tubes'  
      3 = 'Other tube or pelvic problems'  
      4 = 'Endometriosis'  
      5 = 'Semen or sperm problems'  
      6 = 'Any other infertility problems'  
      7 = 'None of these problems'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value DUCHFREQ
      1 = 'Never'  
      2 = 'Once a month or less often'  
      3 = '2-3 times a month'  
      4 = 'Once a week'  
      5 = '2-3 times a week'  
      6 = '4-6 times a week'  
      7 = 'Every day'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value PIDTX
      1 = '1 TIME'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value DIABETES
      1 = 'Yes'  
      3 = 'If volunteered: Borderline or Pre-Diabetes'  
      5 = 'No'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value DHLONGF
      1 = 'Less than one year ago'  
      2 = '1-4 years ago'  
      3 = '5-9 years ago'  
      4 = '10 years ago or longer'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value TREATF
      1 = 'Non-narcotic medicines to treat pain'  
      2 = 'Narcotic medicines to treat pain'  
      3 = 'Hormonal medicines'  
      4 = 'Progesterone releasing IUD or implant'  
      5 = 'Hysterectomy'  
      6 = 'Other surgery'  
      7 = 'Other nonsurgical treatment'  
      8 = 'Complementary or alternative medicines or treatments'  
      9 = 'Never had any of the above treatments for fibroids'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value PCOSSYMP
      1 = 'Irregular menstrual periods'  
      2 = 'Pelvic ultrasound'  
      3 = 'Acne'  
      4 = 'Body hirsutism (excessive hair growth)'  
      5 = 'Blood tests (including measurements of hormones such as FSH/LH, Testosterone, Thyroid stimulating hormone/TSH, or Prolactin)'  
      6 = 'Other tests or symptoms'  
      7 = 'None of these tests or symptoms'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value AGECANCER
      17 = '17 YEARS OR YOUNGER'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value PRECANCER
      1 = 'Abnormal Pap test result, suspicious for cancer, but no real cancer found'  
      2 = 'Pre-cancer (cervical intraepithelial lesion or CIN)'  
      3 = 'Cervical cancer'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value AGEMAM1F
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value REASMAMF
      1 = 'Part of a routine exam'  
      2 = 'Because of a problem or lump'  
      3 = 'Because of family or personal history of cancer'  
      4 = 'Other reason'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value RISKF
      1 = 'A lot'  
      2 = 'A little'  
      3 = 'Not at all'  
      4 = 'No opinion'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value NOHIVTST
      1 = 'You have never been offered an HIV test'  
      2 = 'You are worried about what other people would think if you got tested for HIV'  
      3 = 'It''s unlikely you''ve been exposed to HIV'  
      4 = 'You were afraid to find out if you were HIV positive (that you had HIV)'  
      5 = 'You don''t like needles'  
      20 = 'Some other reason'  
      21 = 'R reported spouse or partner tested negative'  
      22 = 'Never had sexual intercourse'  
      23 = 'No health insurance or coverage, or R couldn''t afford an HIV test'  
      24 = 'Part of blood donation'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value WHYNOGET
      1 = 'You thought the testing site would contact you'  
      2 = 'You were afraid to find out if you were HIV positive (that you had HIV)'  
      3 = 'You didn''t want to know your HIV test results'  
      4 = 'You didn''t know where or how to get your test result'  
      20 = 'Some other reason'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value PLCHIV
      1 = 'Private doctor''s office'  
      2 = 'HMO facility'  
      3 = 'Community health clinic, community clinic, public health clinic'  
      4 = 'Family planning or Planned Parenthood clinic'  
      5 = 'Employer or company clinic'  
      6 = 'School or school-based clinic (including college or university)'  
      7 = 'Hospital outpatient clinic'  
      8 = 'Hospital emergency room'  
      9 = 'Hospital regular room'  
      10 = 'Urgent care center, urgi-care, or walk-in facility'  
      11 = 'Your worksite'  
      12 = 'Your home'  
      13 = 'Military induction or military service site'  
      14 = 'Sexually transmitted disease (STD) clinic'  
      15 = 'Laboratory or blood bank'  
      20 = 'Some other place -- specify'  
      21 = 'Prison or jail'  
      22 = 'Mobile testing or community testing site'  
      23 = 'Drug, alcohol, or rehabilitation center'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value RHHIVT2F
      1 = 'I didn''t want to get tested by a doctor or at an HIV testing site'  
      2 = 'I didn''t want other people to know I am getting tested'  
      3 = 'I wanted to get tested together with someone, before we had sex'  
      4 = 'I wanted to get tested by myself, before having sex'  
      5 = 'I wanted to get tested by myself, after having sex'  
      6 = 'A sex partner asked me to take a rapid home HIV test'  
      20 = 'Other reason'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value HIVTST
      1 = 'Part of medical checkup or surgical procedure (a doctor or medical provider asked for the test)'  
      2 = 'Required for health or life insurance coverage'  
      3 = 'Required for marriage license or to get married'  
      4 = 'Required for military service or a job'  
      5 = 'You wanted to find out if infected or not (you were the one who asked for the test)'  
      6 = 'Someone else suggested you should be tested'  
      7 = 'Intentionally blank (a code used only for females, prenatal testing)'  
      8 = 'You might have been exposed through sex or drug use'  
      9 = 'You might have been exposed in some other way'  
      20 = 'Some other reason - not shown separately'  
      21 = 'Required for immigration or travel'  
      22 = 'Required for incarceration'  
      23 = 'Required for school'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value AIDSTALKF
      1 = 'How HIV/AIDS is transmitted'  
      2 = 'Other sexually transmitted diseases like gonorrhea, herpes, or Hepatitis C'  
      3 = 'The correct use of condoms'  
      4 = 'Needle cleaning/using clean needles'  
      5 = 'Dangers of needle sharing'  
      6 = 'Abstinence from sex (not having sex)'  
      7 = 'Reducing your number of sexual partners'  
      8 = 'Condom use to prevent HIV or STD transmission'  
      9 = 'Safe sex practices (abstinence, condom use, etc)'  
      10 = 'Getting tested and knowing your HIV status'  
      11 = 'Medicines to prevent getting HIV (pre-exposure prophylaxis, also known as PrEP)'  
      20 = 'Some other topic - not shown separately'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value RETROVIR
      1 = 'Definitely true'  
      2 = 'Probably true'  
      3 = 'Probably false'  
      4 = 'Definitely false'  
      5 = 'Don''t know if true or false'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value PREGHIV
      1 = 'Yes'  
      5 = 'No'  
      6 = 'Never went for prenatal care'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value AGE25NRDF
      9 = '9 YEARS OR YOUNGER'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value HPVSEX1F
      1 = 'First intercourse'  
      5 = 'First HPV vaccine shot'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value LKNLK2FMT
      1 = 'Very likely'  
      2 = 'Somewhat likely'  
      3 = 'Not too likely'  
      4 = 'Not likely at all'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value HIGHBP
      1 = 'Yes'  
      5 = 'No'  
      6 = 'If volunteered: Not told results'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value NUTRINFO
      1 = 'Friends'  
      2 = 'Family (such as spouse, mother, mother-in-law, sister)'  
      3 = 'Child''s doctor or other health care provider'  
      4 = 'Child''s daycare provider, nanny, or teacher'  
      5 = 'Websites, blogs, or social media'  
      6 = 'None of the above sources'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value USLPLACE
      1 = 'Private doctor''s office or HMO'  
      2 = 'Community health clinic, community clinic, public health clinic'  
      3 = 'Family planning or Planned Parenthood Clinic'  
      4 = 'Employer or company clinic'  
      5 = 'School or school-based clinic'  
      6 = 'Hospital outpatient clinic'  
      7 = 'Hospital emergency room'  
      8 = 'Hospital regular room'  
      9 = 'Urgent care center, urgi-care, or walk-in facility'  
      10 = 'Sexually transmitted disease (STD) clinic'  
      11 = 'In-store health clinic (like CVS, Target, or Walmart)'  
      20 = 'Some other place'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value COVERHOWF
      1 = 'Private health insurance plan (from employer or workplace; purchased directly; through a state or local government program or community program)'  
      2 = 'Medicaid-additional name(s) for Medicaid in this state: [MEDICAID_FILL]'  
      4 = 'Medi-GAP'  
      5 = 'Military health care including: the VA, CHAMPUS, TRICARE, CHAMP-VA'  
      6 = 'Indian Health Service, or Single Service Plan'  
      7 = 'CHIP'  
      8 = 'State-sponsored health plan'  
      9 = 'Medicare or other government health care'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value NUMNOCOV
      1 = '1 Month'  
      2 = '2 Months'  
      3 = '3 Months'  
      4 = '4 Months'  
      5 = '5 Months'  
      6 = '6 Months'  
      7 = '7 Months'  
      8 = '8 Months'  
      9 = '9 Months'  
      10 = '10 Months'  
      11 = '11 Months'  
      12 = '12 Months'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value RELCURR
      1 = 'No religion'  
      2 = 'Catholic'  
      3 = 'Baptist/Southern Baptist'  
      4 = 'Methodist, Lutheran, Presbyterian, Episcopal'  
      5 = 'Fundamentalist Protestant'  
      6 = 'Other Protestant denomination'  
      7 = 'Protestant - No specific denomination'  
      8 = 'Other religion'  
      9 = 'Refused'  
      10 = 'Don''t know' ;
   value ATTNDF
      1 = 'More than once a week'  
      2 = 'Once a week'  
      3 = '2-3 times a month'  
      4 = 'Once a month (about 12 times a year)'  
      5 = '3-11 times a year'  
      6 = 'Once or twice a year'  
      7 = 'Never'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value RELTRAD
      1 = 'Evangelical Prot'  
      2 = 'Mainline Prot'  
      3 = 'Black Prot'  
      4 = 'Catholic'  
      5 = 'Other religion'  
      6 = 'No religious affiilation'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value FUNDAMF
      1 = 'A born again Christian'  
      2 = 'A charismatic'  
      3 = 'An evangelical'  
      4 = 'A fundamentalist'  
      5 = 'None of the above'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value RELDLIFE
      1 = 'Very important'  
      2 = 'Somewhat important'  
      3 = 'Not important'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value WRK12MOS
      0 = 'No months'  
      1 = '1 Month'  
      2 = '2 Months'  
      3 = '3 Months'  
      4 = '4 Months'  
      5 = '5 Months'  
      6 = '6 Months'  
      7 = '7 Months'  
      8 = '8 Months'  
      9 = '9 Months'  
      10 = '10 Months'  
      11 = '11 Months'  
      12 = '12 Months'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value FPT12MOS
      1 = 'Full-time'  
      2 = 'Part-time'  
      3 = 'Some of each'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value DOLASTWK
      1 = 'Working'  
      2 = 'Working - Maternity leave or temp leave'  
      3 = 'Not working, looking for work'  
      4 = 'Keeping house or taking care of family'  
      5 = 'In school'  
      6 = 'Other'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value NUMJOBF
      1 = '1 job'  
      2 = '2 or more jobs'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value TMWK12MO
      1 = 'Full-time'  
      2 = 'Part-time'  
      3 = 'Some of each'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value SPLSTWKF
      1 = 'Working'  
      2 = 'Working - Paternity leave or temp leave'  
      3 = 'Not working, looking for work'  
      4 = 'Keeping house or taking care of family'  
      5 = 'Other'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value AGDGFMT
      1 = 'Strongly agree'  
      2 = 'Agree'  
      3 = 'Disagree'  
      4 = 'Strongly disagree'  
      5 = 'If R insists: Neither agree nor disagree'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value REACTSLF
      1 = 'Very upset'  
      2 = 'A little upset'  
      3 = 'A little pleased'  
      4 = 'Very pleased'  
      5 = 'If R insists: She wouldn''t care'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value CHBOTHER
      1 = 'A great deal'  
      2 = 'Some'  
      3 = 'A little'  
      4 = 'Not at all'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value ACASILANG
      1 = 'English'  
      2 = 'Spanish'  
      7 = 'Not ascertained' ;
   value GENHEALT
      1 = 'Excellent'  
      2 = 'Very good'  
      3 = 'Good'  
      4 = 'Fair'  
      5 = 'Poor'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value BMICAT
      1 = 'Underweight'  
      2 = 'Normal weight'  
      3 = 'Overweight'  
      4 = 'Obese'  
      5 = 'Undefined BMI'  
      7 = 'Not ascertained' ;
   value TELLWGHT
      1 = 'Underweight'  
      2 = 'Normal weight'  
      3 = 'Overweight'  
      4 = 'Obese'  
      5 = 'Not told'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value ENGSPEAK
      1 = 'Very well'  
      2 = 'Well'  
      3 = 'Not well'  
      4 = 'Not at all'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value CASINUM
      0 = 'NO PREGNANCIES'  
      1 = '1 PREGNANCY'  
      2 = '2 PREGNANCIES'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value CASIGRAD
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value CASISMK
      0 = 'NEVER SMOKED REGULARLY'  
      11 = '11 YEARS OR YOUNGER'  
      12 = '12 YEARS OLD'  
      13 = '13 YEARS OLD'  
      14 = '14 YEARS OLD'  
      15 = '15 YEARS OLD'  
      16 = '16 YEARS OLD'  
      17 = '17 YEARS OLD'  
      18 = '18 YEARS OLD'  
      19 = '19 YEARS OLD'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value SMOKE12F
      1 = 'None'  
      2 = 'About one cigarette a day or less'  
      3 = 'Just a few cigarettes a day, between 2 to 4 cigarettes'  
      4 = 'About half a pack a day, between 5 to 14 cigarettes'  
      5 = 'About a pack a day, between 15 to 24 cigarettes'  
      6 = 'More than a pack a day, 25 or more cigarettes'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value DRINKF
      1 = 'Never'  
      2 = 'Once or twice during the year'  
      3 = 'Several times during the year'  
      4 = 'About once a month'  
      5 = 'About once a week'  
      6 = 'About once a day'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value UNIT30D
      1 = 'Days per week'  
      5 = 'Days per month'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value DRNK30D
      0 = '0 DAYS'  
      1 = '1 DAY'  
      2 = '2 DAYS'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value DRNKNUM
      1 = '1 DRINK'  
      2 = '2 DRINKS'  
      3 = '3 DRINKS'  
      4 = '4 DRINKS'  
      5 = '5 DRINKS'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value BNGE30T
      0 = 'NONE'  
      1 = '1 TIME'  
      2 = '2 TIMES'  
      3 = '3 TIMES'  
      4 = '4 TIMES'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value POTFRF
      1 = 'Never'  
      2 = 'Once or twice during the year'  
      3 = 'Several times during the year'  
      4 = 'About once a month'  
      5 = 'About once a week'  
      6 = 'About once a day or more'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value DRUGFRF
      1 = 'Never'  
      2 = 'Once or twice during the year'  
      3 = 'Several times during the year'  
      4 = 'About once a month or more'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value AGEVAGR
      10 = '10 YEARS OR YOUNGER'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value WHYCONDL
      1 = 'To prevent pregnancy'  
      2 = 'To prevent diseases like syphilis, gonorrhea or AIDS'  
      3 = 'For both reasons'  
      4 = 'For some other reason'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value Y1N5NAC
      1 = 'YES'  
      5 = 'NO'  
      7 = 'Not ascertained' ;
   value TIMING
      1 = 'Before first vaginal intercourse'  
      3 = 'After first vaginal intercourse'  
      5 = 'Same occasion'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value PARTLIF
      1 = '1 PARTNER'  
      2 = '2 PARTNERS'  
      3 = '3 PARTNERS'  
      4 = '4 PARTNERS'  
      5 = '5 PARTNERS'  
      6 = '6 PARTNERS'  
      7 = '7 PARTNERS'  
      8 = '8 PARTNERS'  
      9 = '9 PARTNERS'  
      10 = '10 PARTNERS'  
      50 = '50 OR MORE PARTNERS'  
      997 = 'Not ascertained'  
      998 = 'Refused'  
      999 = 'Don''t know' ;
   value PRTS12MB
      0 = '0 PARTNERS'  
      1 = '1 PARTNER'  
      2 = '2 PARTNERS'  
      3 = '3 PARTNERS'  
      4 = '4 PARTNERS'  
      5 = '5 PARTNERS'  
      6 = '6 PARTNERS'  
      7 = '7 OR MORE PARTNERS'  
      997 = 'Not ascertained'  
      998 = 'Refused'  
      999 = 'Don''t know' ;
   value NNONMONOG1F
      1 = '1 partner'  
      2 = '2 or more partners'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value NNONMONOG2F
      1 = '1 other partner besides you'  
      2 = '2 or more other partners besides you'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value PRTLIFEF
      0 = '0 PARTNERS'  
      1 = '1 PARTNER'  
      2 = '2 PARTNERS'  
      3 = '3 PARTNERS'  
      4 = '4 PARTNERS'  
      5 = '5 PARTNERS'  
      6 = '6 PARTNERS'  
      7 = '7 PARTNERS'  
      8 = '8 PARTNERS'  
      9 = '9 PARTNERS'  
      10 = '10 OR MORE PARTNERS'  
      997 = 'Not ascertained'  
      998 = 'Refused'  
      999 = 'Don''t know' ;
   value PRT12F
      0 = '0 PARTNERS'  
      1 = '1 PARTNER'  
      2 = '2 PARTNERS'  
      3 = '3 OR MORE PARTNERS'  
      997 = 'Not ascertained'  
      998 = 'Refused'  
      999 = 'Don''t know' ;
   value SAMESEX1F
      1 = 'UNDER 15 YEARS'  
      2 = '15-19 YEARS'  
      3 = '20-29 YEARS'  
      4 = '30-49 YEARS'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value FSAMEREL
      1 = 'Married to her'  
      2 = 'Engaged to her'  
      3 = 'Living together in a sexual relationship, but not engaged'  
      4 = 'Going with her or going steady'  
      5 = 'Going out with her once in a while'  
      6 = 'Just friends'  
      7 = 'Had just met her'  
      8 = 'Something else'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value MALFEMNAF
      1 = 'Male'  
      2 = 'Female'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value ATTRACT
      1 = 'Only attracted to males'  
      2 = 'Mostly attracted to males'  
      3 = 'Equally attracted to males and females'  
      4 = 'Mostly attracted to females'  
      5 = 'Only attracted to females'  
      6 = 'Not sure'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value ORIENT_A
      1 = 'Heterosexual or straight'  
      2 = 'Homosexual, gay, or lesbian'  
      3 = 'Bisexual'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value ORIENT_B
      1 = 'Lesbian or gay'  
      2 = 'Straight, that is, not lesbian or gay'  
      3 = 'Bisexual'  
      4 = 'Something else'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value TIMALON
      1 = 'Yes'  
      5 = 'No'  
      6 = 'Did not have a health care visit in the past 12 months'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value INCWMYF
      1 = 'Week'  
      2 = 'Month'  
      3 = 'Year'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value EARN
      1 = 'Under $96 (weekly)/Under $417 (monthly)/Under $5,000 (yearly)'  
      2 = '$96-$143 (weekly)/$417-624 (monthly)/$5,000-7,499 (yearly)'  
      3 = '$144-191 (weekly)/$625-832 (monthly)/$7,500-9,999 (yearly)'  
      4 = '$192-239 (weekly)/$833-1,041 (monthly)/$10,000-12,499 (yearly)'  
      5 = '$240-288 (weekly)/$1,042-1,249 (monthly)/$12,500-14,999 (yearly)'  
      6 = '$289-384 (weekly)/$1,250-1,666 (monthly)/$15,000-19,999 (yearly)'  
      7 = '$385-480 (weekly)/$1,667-2,082 (monthly)/$20,000-24,999 (yearly)'  
      8 = '$481-576 (weekly)/$2,083-2,499 (monthly)/$25,000-29,999 (yearly)'  
      9 = '$577-672 (weekly)/$2,500-2,916 (monthly)/$30,000-34,999 (yearly)'  
      10 = '$673-768 (weekly)/$2,917-3,332 (monthly)/$35,000-39,999 (yearly)'  
      11 = '$769-961 (weekly)/$3,333-4,166 (monthly)/$40,000-49,999 (yearly)'  
      12 = '$962-1,153 (weekly)/$4,167-4,999 (monthly)/$50,000-59,999 (yearly)'  
      13 = '$1,154-1,441 (weekly)/$5,000-6,249 (monthly)/$60,000-74,999 (yearly)'  
      14 = '$1,442-1,922 (weekly)/$6,250-8,332 (monthly)/$75,000-99,999 (yearly)'  
      15 = '$1,923 or more (weekly)/$8,333 or more (monthly)/$100,000 or more (yearly)'  
      97 = 'Not ascertained'  
      98 = 'Refused'  
      99 = 'Don''t know' ;
   value FMINCDK1F
      1 = 'Less than $50,000'  
      5 = '$50,000 or more'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value P_ASTYP
      1 = 'Public assist prog, e.g. AFDC or ADC'  
      2 = 'General/Emergency/Other assistance'  
      7 = 'Not ascertained'  
      8 = 'Refused'  
      9 = 'Don''t know' ;
   value AGERFF
      15 = '15 YEARS'  
      16 = '16 YEARS'  
      17 = '17 YEARS'  
      18 = '18 YEARS'  
      19 = '19 YEARS'  
      20 = '20 YEARS'  
      21 = '21 YEARS'  
      22 = '22 YEARS'  
      23 = '23 YEARS'  
      24 = '24 YEARS'  
      25 = '25 YEARS'  
      26 = '26 YEARS'  
      27 = '27 YEARS'  
      28 = '28 YEARS'  
      29 = '29 YEARS'  
      30 = '30 YEARS'  
      31 = '31 YEARS'  
      32 = '32 YEARS'  
      33 = '33 YEARS'  
      34 = '34 YEARS'  
      35 = '35 YEARS'  
      36 = '36 YEARS'  
      37 = '37 YEARS'  
      38 = '38 YEARS'  
      39 = '39 YEARS'  
      40 = '40 YEARS'  
      41 = '41 YEARS'  
      42 = '42 YEARS'  
      43 = '43 YEARS'  
      44 = '44 YEARS'  
      45 = '45 YEARS'  
      46 = '46 YEARS'  
      47 = '47 YEARS'  
      48 = '48 YEARS' ;
   value FMARITAL
      1 = 'MARRIED TO A PERSON OF THE OPPOSITE SEX'  
      2 = 'WIDOWED'  
      3 = 'DIVORCED OR ANNULLED'  
      4 = 'SEPARATED'  
      5 = 'NEVER MARRIED' ;
   value RMARITAL
      1 = 'CURRENTLY MARRIED TO A PERSON OF THE OPPOSITE SEX'  
      2 = 'NOT MARRIED BUT LIVING WITH OPP SEX PARTNER'  
      3 = 'WIDOWED'  
      4 = 'DIVORCED OR ANNULLED'  
      5 = 'SEPARATED FOR REASONS OF MARITAL DISCORD'  
      6 = 'NEVER BEEN MARRIED' ;
   value HIEDUC
      5 = '9TH GRADE OR LESS'  
      6 = '10TH GRADE'  
      7 = '11TH GRADE'  
      8 = '12TH GRADE, NO DIPLOMA (NOR GED)'  
      9 = 'HIGH SCHOOL GRADUATE (DIPLOMA OR GED)'  
      10 = 'SOME COLLEGE BUT NO DEGREE'  
      11 = 'ASSOCIATE DEGREE IN COLLEGE/UNIVERSITY'  
      12 = 'BACHELOR''S DEGREE'  
      13 = 'MASTER''S DEGREE'  
      14 = 'DOCTORATE DEGREE'  
      15 = 'PROFESSIONAL DEGREE' ;
   value HISPANIC
      1 = 'HISPANIC'  
      2 = 'NON-HISPANIC' ;
   value RACE
      1 = 'BLACK'  
      2 = 'WHITE'  
      3 = 'OTHER' ;
   value HISPRACE2F
      1 = 'Hispanic'  
      2 = 'Non-Hispanic White, Single Race'  
      3 = 'Non-Hispanic Black, Single Race'  
      4 = 'Non-Hispanic Other or Multiple Race' ;
   value NUMKDHH
      0 = 'NO CHILDREN'  
      1 = '1 CHILD'  
      2 = '2 CHILDREN'  
      3 = '3 CHILDREN'  
      4 = '4 CHILDREN OR MORE' ;
   value NUMFMHH
      0 = 'NO FAMILY MEMBERS'  
      1 = '1 FAMILY MEMBER'  
      2 = '2 FAMILY MEMBERS'  
      3 = '3 FAMILY MEMBERS'  
      4 = '4 FAMILY MEMBERS'  
      5 = '5 FAMILY MEMBERS'  
      6 = '6 FAMILY MEMBERS'  
      7 = '7 FAMILY MEMBERS OR MORE' ;
   value HHFAMTYP
      1 = 'No spouse/partner and no child(ren) (of R) 18 or younger'  
      2 = 'Spouse/partner, but no child(ren) (of R) 18 or younger'  
      3 = 'Spouse and R''s child(ren) 18 or younger'  
      4 = 'Cohabiting partner and R''s child(ren) 18 or younger'  
      5 = 'No spouse/partner, but child(ren) of R, 18 or younger' ;
   value HHPARTYP
      1 = 'Both biological or both adoptive parents'  
      2 = 'Biological and step- or adoptive parent'  
      3 = 'Single parent (biological, adoptive, or stepparent)'  
      4 = 'Other' ;
   value NCHILDHH
      0 = 'No children of respondent age 18 or younger in the household'  
      1 = '1 of respondent''s children 18 or younger in the household'  
      2 = '2 of respondent''s children 18 or younger in the household'  
      3 = '3 or more of respondent''s children 18 or younger in the household' ;
   value HHKIDTYP
      0 = 'No child(ren) 18 or younger in HH or only older child(ren)'  
      1 = 'At least one biological child (of R''s) under 18 in HH, no nonbiological child(ren)'  
      2 = 'Any non-biological child (of R''s) 18 or younger in HH' ;
   value CSPBIO
      0 = 'No joint biological children'  
      1 = '1 joint biological child'  
      2 = '2 joint biological children'  
      3 = '3 or more joint biological children' ;
   value CSPNOT
      0 = 'No such child'  
      1 = '1 or more such children' ;
   value INTCTFAM
      1 = 'TWO BIOLOGICAL OR ADOPTIVE PARENTS FROM BIRTH'  
      2 = 'ANYTHING OTHER THAN 2 BIOLOGICAL OR ADOPTIVE PARENTS FROM BIRTH' ;
   value PARAGEF
      1 = 'R LIVED WITH BOTH BIOLOGICAL OR BOTH ADOPTIVE PARENTS AT AGE 14'  
      2 = 'R LIVED WITH BIOLOGICAL MOTHER AND STEPFATHER AT AGE 14'  
      3 = 'R LIVED IN ANY OTHER PARENTAL SITUATION OR A NONPARENTAL SITUATION AT AGE 14' ;
   value EDUCMOM
      1 = 'Less than high school'  
      2 = 'High school graduate or GED'  
      3 = 'Some college, including 2-year degrees'  
      4 = 'Bachelor''s degree or higher'  
      95 = 'No mother-figure identified' ;
   value AGEMOMBF
      1 = 'LESS THAN 18 YEARS'  
      2 = '18-19 YEARS'  
      3 = '20-24 YEARS'  
      4 = '25-29 YEARS'  
      5 = '30 OR OLDER'  
      95 = 'NO MOTHER OR MOTHER-FIGURE'  
      96 = 'MOTHER-FIGURE HAD NO CHILDREN' ;
   value IMPFLG
      0 = 'QUESTIONNAIRE DATA (NOT IMPUTED)'  
      1 = 'MULTIPLE REGRESSION IMPUTATION'  
      2 = 'LOGICAL IMPUTATION' ;
   value RCURPREG
      1 = 'YES (CURRENTLY PREGNANT)'  
      2 = 'NO (NOT CURRENTLY PREGNANT)' ;
   value PREGNUMF
      0 = 'NONE'  
      1 = '1 PREGNANCY'  
      2 = '2 PREGNANCIES'  
      3 = '3 PREGNANCIES'  
      4 = '4 PREGNANCIES'  
      5 = '5 PREGNANCIES'  
      6 = '6 PREGNANCIES' ;
   value TPRGOUTF
      0 = 'NONE'  
      1 = '1 PREGNANCY'  
      2 = '2 PREGNANCIES' ;
   value LBPREGS
      0 = 'NONE'  
      1 = '1 PREGNANCY'  
      2 = '2 PREGNANCIES'  
      3 = '3 PREGNANCIES' ;
   value PARITY
      0 = '0 BABIES'  
      1 = '1 BABY'  
      2 = '2 BABIES'  
      3 = '3 BABIES'  
      4 = '4 BABIES' ;
   value BIRTHS5F
      0 = '0 BIRTHS'  
      1 = '1 BIRTH'  
      2 = '2 BIRTHS'  
      3 = '3 BIRTHS'  
      4 = '4 BIRTHS' ;
   value OUTCOMRF
      1 = 'LIVE BIRTH'  
      2 = 'INDUCED ABORTION'  
      3 = 'STILLBIRTH'  
      4 = 'MISCARRIAGE'  
      5 = 'ECTOPIC PREGNANCY'  
      6 = 'CURRENT PREGNANCY' ;
   value MARPRGF
      1 = 'Married'  
      2 = 'Divorced'  
      3 = 'Widowed'  
      4 = 'Separated'  
      5 = 'Never married' ;
   value RMARFMT
      1 = 'Married'  
      2 = 'Divorced'  
      3 = 'Widowed'  
      4 = 'Separated'  
      5 = 'Cohabiting'  
      6 = 'Never married, not cohabiting' ;
   value CEBOW
      0 = 'NONE'  
      1 = '1 CHILD'  
      2 = '2 CHILDREN' ;
   value LASTPREG
      1 = 'Less than 1 year'  
      2 = '1 to 5 years ago'  
      3 = '5 years or longer ago' ;
   value PREGINT
      1 = '0-5 months'  
      2 = '6-11 months'  
      3 = '12-17 months'  
      4 = '18-23 months'  
      5 = '24-59 months'  
      6 = '60 months or longer'  
      7 = 'Could not be defined' ;
   value FMARNO
      0 = 'NEVER BEEN MARRIED'  
      1 = '1 TIME'  
      2 = '2 TIMES'  
      3 = '3 TIMES'  
      4 = '4 TIMES' ;
   value MARENDF
      1 = 'DIVORCED OR ANNULLED'  
      2 = 'SEPARATED'  
      3 = 'WIDOWED' ;
   value MARCATA
      1 = '0-12 months'  
      2 = '13-24 months'  
      3 = '25-36 months'  
      4 = '37-48 months'  
      5 = 'More than 4 years' ;
   value MARCATB
      1 = '0-12 months'  
      2 = '13-24 months'  
      3 = '25-36 months'  
      4 = 'More than 3 years'  
      888 = 'First birth occurred before first marriage' ;
   value MARCATC
      1 = '0-12 months'  
      2 = '13-24 months'  
      3 = '25-36 months'  
      4 = 'More than 3 years'  
      995 = 'First conception before marriage'  
      996 = 'Has been pregnant, but has never been married' ;
   value MARCATD
      1 = '0-12 months'  
      2 = '13-24 months'  
      3 = '25-36 months'  
      4 = 'More than 3 years'  
      996 = 'Has been married but has never been pregnant' ;
   value COHEVER
      1 = 'YES, EVER COHABITED (LIVED WITH A MAN OUTSIDE OF MARRIAGE)'  
      2 = 'NO, NEVER COHABITED (LIVED WITH A MAN OUTSIDE OF MARRIAGE)' ;
   value EVMARCOH
      1 = 'YES, EVER MARRIED OR COHABITED'  
      2 = 'NO, NEVER MARRIED OR COHABITED' ;
   value COHNUM2F
      0 = 'None'  
      1 = 'One'  
      2 = 'Two'  
      3 = 'THREE'  
      4 = 'Four'  
      5 = 'Five or more' ;
   value COHSTAT
      1 = 'NEVER COHABITED OUTSIDE OF MARRIAGE'  
      2 = 'FIRST COHABITED BEFORE FIRST MARRIAGE'  
      3 = 'FIRST COHABITED AFTER FIRST MARRIAGE' ;
   value COHOUT
      1 = 'INTACT COHABITATION'  
      2 = 'INTACT MARRIAGE'  
      3 = 'DISSOLVED MARRIAGE'  
      4 = 'DISSOLVED COHABITATION' ;
   value HADSEX
      1 = 'YES, R EVER HAD INTERCOURSE'  
      2 = 'NO, R NEVER HAD INTERCOURSE' ;
   value SEXONCE
      1 = 'YES (R HAS HAD SEX ONLY ONCE)'  
      2 = 'NO (R HAS HAD SEX MORE THAN ONCE)' ;
   value SEXEVER
      1 = 'YES (R HAS HAD SEX AFTER 1ST PERIOD)'  
      2 = 'NO (NO PERIOD, NO SEX AT ALL, OR NO SEX SINCE 1ST PERIOD)' ;
   value SEX1RECF
      10 = '10 YEARS OR YOUNGER' ;
   value VRY1STSX
      9996 = 'RESPONDED SHE NEVER HAD INTERCOURSE'  
      9997 = 'Not ascertained' ;
   value DATESEX1F
      9595 = 'NEVER HAD A MENSTRUAL PERIOD BUT HAS HAD INTERCOURSE'  
      9997 = 'Not ascertained' ;
   value MARCATE
      1 = '0-12 months'  
      2 = '13-24 months'  
      3 = '25-36 months'  
      4 = 'More than 3 years'  
      5 = 'First intercourse after first marriage' ;
   value MARCATF
      1 = '0-12 months'  
      2 = '13-24 months'  
      3 = '25-36 months'  
      4 = 'More than 3 years'  
      5 = 'First intercourse after menarche after first marriage' ;
   value MARCATG
      1 = '0-12 months'  
      2 = '13-24 months'  
      3 = '25-36 months'  
      4 = 'More than 3 years'  
      5 = 'First intercourse after first marriage or cohabitation' ;
   value SEXOUT
      1 = 'Intact cohabitation'  
      2 = 'Intact marriage'  
      3 = 'Dissolved marriage'  
      4 = 'Dissolved cohabitation'  
      5 = 'Never married or cohabited with 1st sexual partner' ;
   value FPDUR
      997 = 'Only had sex once with first partner' ;
   value PARTS1YR
      0 = 'NONE'  
      1 = '1 PARTNER'  
      2 = '2 PARTNERS'  
      3 = '3 PARTNERS'  
      4 = '4 PARTNERS'  
      5 = '5 PARTNERS'  
      6 = '6 PARTNERS'  
      7 = '7 OR MORE PARTNERS' ;
   value NUMP3MOS
      0 = 'NONE'  
      1 = '1 PARTNER' ;
   value LSEXRAGE
      10 = '10 YEARS OR YOUNGER' ;
   value PARTDURF
      997 = 'Only had sex once with partner' ;
   value RELATPF
      1 = 'Married to him'  
      2 = 'Engaged to him'  
      3 = 'Living together in a sexual relationship, but not engaged'  
      4 = 'Going out with him or going steady'  
      5 = 'Going out with him once in a while'  
      6 = 'Just friends'  
      7 = 'Had just met him'  
      8 = 'Something else' ;
   value LIFPRTNR
      0 = 'NONE'  
      1 = '1 PARTNER'  
      2 = '2 PARTNERS'  
      3 = '3 PARTNERS'  
      4 = '4 PARTNERS'  
      5 = '5 PARTNERS'  
      6 = '6 PARTNERS'  
      7 = '7 PARTNERS'  
      8 = '8 PARTNERS'  
      9 = '9 PARTNERS'  
      50 = '50 OR MORE PARTNERS' ;
   value STRLOPER
      1 = 'TUBAL LIGATION OR STERILIZATION'  
      2 = 'HYSTERECTOMY'  
      3 = 'VASECTOMY'  
      4 = 'OTHER OPERATION OR TYPE UNKNOWN'  
      5 = 'NOT SURGICALLY STERILE' ;
   value FECUND
      1 = 'SURGICALLY STERILE, CONTRACEPTIVE'  
      2 = 'SURGICALLY STERILE, NONCONTRACEPTIVE'  
      3 = 'STERILE, NONSURGICAL'  
      4 = 'SUBFECUND'  
      5 = 'LONG INTERVAL (INFERTILE FOR 36+ MONTHS)'  
      6 = 'FECUND' ;
   value ANYBCF
      1 = 'Yes, at least one month of method use'  
      2 = 'No, no months of method use' ;
   value NOSEX36F
      1 = 'No, no months of non-intercourse -- intercourse in all months'  
      2 = 'Yes, one or more months of non-intercourse' ;
   value INFERT
      1 = 'SURGICALLY STERILE'  
      2 = 'INFERTILE'  
      3 = 'FECUND' ;
   value NOSEX12F
      0 = 'NONE'  
      1 = '1 MONTH'  
      2 = '2 MONTHS'  
      3 = '3 MONTHS'  
      4 = '4 MONTHS'  
      5 = '5 MONTHS'  
      6 = '6 MONTHS'  
      7 = '7 MONTHS'  
      8 = '8 MONTHS'  
      9 = '9 MONTHS'  
      10 = '10 MONTHS'  
      11 = '11 MONTHS'  
      12 = '12 MONTHS'  
      95 = 'NEVER HAD INTERCOURSE' ;
   value CONSTATF
      1 = 'Female sterilization'  
      2 = 'Male sterilization'  
      3 = 'Norplant or Implanon implant'  
      5 = 'Depo-Provera (injectable)'  
      6 = 'Pill'  
      7 = 'Contraceptive Patch'  
      8 = 'Contraceptive Ring'  
      9 = 'Emergency contraception'  
      10 = 'IUD'  
      11 = 'Diaphragm (with or w/out jelly or cream)'  
      12 = '(Male) Condom'  
      13 = 'Female condom/vaginal pouch'  
      14 = 'Foam'  
      15 = 'Cervical Cap'  
      16 = 'Today(TM) Sponge'  
      17 = 'Suppository or insert'  
      18 = 'Jelly or cream (not with diaphragm)'  
      19 = 'Periodic abstinence: NFP, cervical mucus test or temperature rhythm'  
      20 = 'Periodic abstinence: calendar rhythm'  
      21 = 'Withdrawal'  
      22 = 'Other method'  
      30 = 'Pregnant'  
      31 = 'Seeking Pregnancy'  
      32 = 'Postpartum'  
      33 = 'Sterile--nonsurgical--female'  
      34 = 'Sterile--nonsurgical--male'  
      35 = 'Sterile--surgical--female (noncontraceptive)'  
      36 = 'Sterile--surgical--male (noncontraceptive)'  
      37 = '[code not used]'  
      38 = 'Sterile--unknown reasons-male'  
      39 = '[code not used]'  
      40 = 'Other nonuser--never had intercourse since first period'  
      41 = 'Other nonuser--has had intercourse, but not in the 3 months prior to interview'  
      42 = 'Other nonuser--had intercourse in the 3 months prior to interview'  
      88 = 'Inapplicable (no 2nd, 3rd, or 4th method reported)' ;
   value SEX1MTHDF
      1 = 'Pill'  
      2 = 'Condom'  
      3 = 'Partner''s vasectomy'  
      4 = 'Female sterilizing operation/tubal ligation'  
      5 = 'Withdrawal'  
      6 = 'Depo-Provera, injectables'  
      7 = 'Implant (Norplant, Implanon, or Nexplanon)'  
      8 = 'Calendar rhythm, Standard Days, or Cycle Beads method'  
      9 = 'Safe period by temperature or cervical mucus test (Two Day, Billing Ovulation, or Sympto-thermal Method)'  
      10 = 'Diaphragm'  
      11 = 'Female condom, vaginal pouch'  
      12 = 'Foam'  
      13 = 'Jelly or cream'  
      14 = 'Cervical cap'  
      15 = 'Suppository, insert'  
      16 = 'Today sponge'  
      17 = 'IUD, coil, loop'  
      18 = 'Emergency contraception'  
      19 = 'Other method'  
      20 = 'Respondent sterile (aside from sterilizing operation above)'  
      21 = 'Respondent sterile (aside from vasectomy above)'  
      22 = 'Lunelle injectable'  
      23 = 'Contraceptive patch'  
      24 = 'Contraceptive ring'  
      95 = 'Never used a method'  
      96 = 'Did not use a method at 1st intercourse' ;
   value MTHUSE12F
      1 = 'USED A METHOD'  
      2 = 'DID NOT USE A METHOD'  
      95 = 'NEVER USED A METHOD' ;
   value METH12MF
      1 = 'Pill'  
      2 = 'Condom'  
      3 = 'Partner''s vasectomy'  
      4 = 'Female sterilizing operation/tubal ligation'  
      5 = 'Withdrawal'  
      6 = 'Depo-Provera injectable'  
      7 = 'Implant (Norplant, Implanon, or Nexplanon)'  
      8 = 'Calendar rhythm, Standard Days, or Cycle Beads method'  
      9 = 'Safe period by temperature or cervical mucus test (Two Day, Billings Ovulation, or Sympto-thermal Method)'  
      10 = 'Diaphragm'  
      11 = 'Female condom, vaginal pouch'  
      12 = 'Foam'  
      13 = 'Jelly or cream'  
      14 = 'Cervical cap'  
      15 = 'Suppository, insert'  
      16 = 'Today sponge'  
      17 = 'IUD, coil, loop'  
      18 = 'Emergency contraception'  
      19 = 'Other method'  
      20 = 'Respondent sterile (aside from sterilizing operation above)'  
      21 = 'Respondent''s partner sterile (aside from vasectomy above)'  
      22 = 'Lunelle injectable'  
      23 = 'Contraceptive patch'  
      24 = 'Contraceptive ring' ;
   value MTHUSE3F
      1 = 'USED A METHOD AT LAST INTERCOURSE IN PAST 3 MONTHS'  
      2 = 'DID NOT USE A METHOD AT LAST INTERCOURSE IN PAST 3 MONTHS'  
      95 = 'NEVER USED A METHOD' ;
   value METH3MF
      1 = 'Pill'  
      2 = 'Condom'  
      3 = 'Partner''s vasectomy'  
      4 = 'Female sterilizing operation/tubal ligation'  
      5 = 'Withdrawal'  
      6 = 'Depo-Provera, injectables'  
      7 = 'Implant (Norplant, Implanon, or Nexplanon)'  
      8 = 'Calendar rhythm, Standard Days, or Cycle Beads method'  
      9 = 'Safe period by temperature or cervical mucus test (Two Day, Billings Ovulation, or Sympto-thermal Method)'  
      10 = 'Diaphragm'  
      11 = 'Female condom, vaginal pouch'  
      12 = 'Foam'  
      13 = 'Jelly or cream'  
      14 = 'Cervical cap'  
      15 = 'Suppository, insert'  
      16 = 'Today sponge'  
      17 = 'IUD, coil, loop'  
      18 = 'Emergency contraception'  
      19 = 'Other method'  
      20 = 'Respondent sterile (aside from sterilizing operation above)'  
      21 = 'Respondent''s partner sterile (aside from vasectomy above)'  
      22 = 'Lunelle injectable'  
      23 = 'Contraceptive patch'  
      24 = 'Contraceptive ring' ;
   value FMETHODF
      1 = 'Pill'  
      2 = 'Condom'  
      3 = 'Partner''s vasectomy'  
      4 = 'Female sterilizing operation/tubal ligation'  
      5 = 'Withdrawal'  
      6 = 'Depo-Provera, injectables'  
      7 = 'Hormonal implant (Norplant, Implanon, or Nexplanon)'  
      8 = 'Calendar rhythm, Standard Days, or Cycle Beads method'  
      9 = 'Safe period by temperature or cervical mucus test (Two Day, Billings Ovulation, or Sympto-thermal Method)'  
      10 = 'Diaphragm'  
      11 = 'Female condom, vaginal pouch'  
      12 = 'Foam'  
      13 = 'Jelly or cream'  
      14 = 'Cervical cap'  
      15 = 'Suppository, insert'  
      16 = 'Today sponge'  
      17 = 'IUD, coil, loop'  
      18 = 'Emergency contraception'  
      19 = 'Other method'  
      20 = 'Respondent sterile (aside from sterilizing operation above)'  
      21 = 'Respondent''s partner sterile (aside from vasectomy above)'  
      22 = 'Lunelle injectable'  
      23 = 'Contraceptive patch'  
      24 = 'Contraceptive ring' ;
   value OWWNTF
      1 = 'LATER, OVERDUE'  
      2 = 'RIGHT TIME'  
      3 = 'TOO SOON, MISTIMED'  
      4 = 'DIDN''T CARE, INDIFFERENT'  
      5 = 'UNWANTED'  
      6 = 'DON''T KNOW, NOT SURE' ;
   value NWWANTRPF
      1 = 'Later, overdue'  
      2 = 'Right time'  
      3 = 'Too soon: by less than 2 years'  
      4 = 'Too soon: by 2 years or more'  
      5 = 'Didn''t care, indifferent'  
      6 = 'Unwanted'  
      7 = 'Don''t know, not sure' ;
   value WANTP5F
      0 = 'No wanted pregnancies in last 5 years' ;
   value FPTIT
      1 = 'TITLE X CLINIC'  
      2 = 'NON-TITLE X CLINIC' ;
   value SRCSRV
      1 = 'CLINIC: TITLE X=YES HEALTH DEPT=YES'  
      2 = 'CLINIC: TITLE X=YES HEALTH DEPT=NO'  
      3 = 'CLINIC: TITLE X=NO HEALTH DEPT=YES'  
      4 = 'CLINIC: TITLE X=NO HEALTH DEPT=NO'  
      5 = 'CLINIC: TITLE X=YES AGENCY UNKNOWN'  
      6 = 'CLINIC: TITLE X=NO AGENCY UNKNOWN'  
      7 = 'EMPLOYER OR COMPANY CLINIC'  
      8 = 'PRIVATE DOCTOR''S OFFICE OR HMO'  
      9 = 'HOSPITAL EMERGENCY REGULAR ROOM/REGULAR ROOM/URGENT CARE'  
      10 = 'SOME OTHER PLACE' ;
   value INTENT
      1 = 'R INTENDS TO HAVE (MORE) CHILDREN'  
      2 = 'R DOES NOT INTEND TO HAVE (MORE) CHILDREN'  
      3 = 'R DOES NOT KNOW HER INTENT' ;
   value ADDEXP
      0 = 'NO ADDITIONAL BIRTHS'  
      5 = '.5 ADDITIONAL BIRTHS'  
      10 = '1 ADDITIONAL BIRTH'  
      15 = '1.5 ADDITIONAL BIRTHS'  
      20 = '2 ADDITIONAL BIRTHS'  
      25 = '2.5 ADDITIONAL BIRTHS'  
      30 = '3 ADDITIONAL BIRTHS'  
      35 = '3.5 ADDITIONAL BIRTHS'  
      40 = '4 ADDITIONAL BIRTHS'  
      45 = '4.5 ADDITIONAL BIRTHS'  
      50 = '5 ADDITIONAL BIRTHS'  
      55 = '5.5 ADDITIONAL BIRTHS'  
      60 = '6 ADDITIONAL BIRTHS'  
      65 = '6.5 ADDITIONAL BIRTHS'  
      70 = '7 ADDITIONAL BIRTHS'  
      75 = '7.5 ADDITIONAL BIRTHS'  
      80 = '8 ADDITIONAL BIRTHS'  
      85 = '8.5 ADDITIONAL BIRTHS'  
      90 = '9 ADDITIONAL BIRTHS'  
      95 = '9.5 ADDITIONAL BIRTHS' ;
   value REPORTF
      1 = 'REPORTED'  
      2 = 'NOT REPORTED' ;
   value EVHIVTST
      0 = 'NO HIV TEST REPORTED'  
      1 = 'YES, ONLY AS PART OF BLOOD DONATION'  
      2 = 'YES, ONLY OUTSIDE OF BLOOD DONATION'  
      3 = 'YES, IN BOTH CONTEXTS' ;
   value FPTITHIV
      1 = 'Clinic: Title X=yes; health department=yes'  
      2 = 'Clinic: Title X=yes; health department=no'  
      3 = 'Clinic: Title X=no; health department=yes'  
      4 = 'Clinic: Title X=no; health department=no'  
      5 = 'Clinic: Title X=yes; agency unknown'  
      6 = 'Clinic: Title X=no; agency unknown'  
      7 = 'Employer or company clinic/worksite'  
      8 = 'Private Doctor''s office or HMO'  
      9 = 'Hospital emergency room/regular room/urgent care'  
      10 = 'Home'  
      11 = 'Military site'  
      12 = 'Lab or blood bank'  
      13 = 'Some other place' ;
   value CURR_INS
      1 = 'Currently covered by private health insurance or Medi-Gap'  
      2 = 'Currently covered by Medicaid, CHIP, or a state-sponsored health plan'  
      3 = 'Currently covered by Medicare, military health care, or other government health care'  
      4 = 'Currently covered only by a single-service plan, only by the Indian Health Service, or currently not covered by health insurance' ;
   value METRO
      1 = 'PRINCIPAL CITY OF MSA'  
      2 = 'OTHER MSA'  
      3 = 'NOT MSA' ;
   value RELIGION
      1 = 'NO RELIGION'  
      2 = 'CATHOLIC'  
      3 = 'PROTESTANT'  
      4 = 'OTHER RELIGIONS' ;
   value LABORFOR
      1 = 'Working full-time'  
      2 = 'Working part-time'  
      3 = 'Working, but on vacation, strike, or had temporary illness'  
      4 = 'Working - maternity or family leave'  
      5 = 'Not working but looking for work'  
      6 = 'In school'  
      7 = 'Keeping house'  
      8 = 'Caring for family'  
      9 = 'Other' ;
   value POVERTY
      50 = '50 percent of poverty level'  
      700 = '700 percent of poverty level or greater' ;
   value TOTINCR
      1 = 'Under $5000'  
      2 = '$5000-$7499'  
      3 = '$7500-$9999'  
      4 = '$10,000-$12,499'  
      5 = '$12,500-$14,999'  
      6 = '$15,000-$19,999'  
      7 = '$20,000-$24,999'  
      8 = '$25,000-$29,999'  
      9 = '$30,000-$34,999'  
      10 = '$35,000-$39,999'  
      11 = '$40,000-$49,999'  
      12 = '$50,000-$59,999'  
      13 = '$60,000-$74,999'  
      14 = '$75,000-$99,999'  
      15 = '$100,000 or more' ;
   value PUBASSIS
      1 = 'Yes (received public assistance in [INTERVIEW YEAR-1])'  
      2 = 'No (did not receive public assistance in [INTERVIEW YEAR-1])' ;
   value SECU
      1 = '1'  
      2 = '2'  
      3 = '3'  
      4 = '4' ;
   value CMJAN3YRF
      1345 = 'January 2012'  
      1357 = 'January 2013'  
      1369 = 'January 2014'  
      1381 = 'January 2015'  
      1393 = 'January 2016' ;
   value CMJAN4YRF
      1333 = 'January 2011'  
      1345 = 'January 2012'  
      1357 = 'January 2013'  
      1369 = 'January 2014'  
      1381 = 'January 2015' ;
   value CMJAN5YRF
      1321 = 'January 2010'  
      1333 = 'January 2011'  
      1345 = 'January 2012'  
      1357 = 'January 2013'  
      1369 = 'January 2014' ;
   value PHASE
      1 = 'First 10 weeks of quarter'  
      2 = 'Last 2 weeks of quarter (double sample)' ;


* SAS DATA, INFILE, INPUT STATEMENTS;

DATA NSFG.FemRespData_2017_2019;
INFILE "/home/u43676684/NSFG/Data/2017_2019_FemRespData.dat" LRECL=3839;
INPUT
   CASEID  1-5              RSCRNINF  6              RSCRAGE  7-8          
   RSCRHISP  9              RSCRRACE  10             AGE_A  11-12          
   AGE_R  13-14             AGESCRN  15-16           HISP  17              
   HISPGRP  18              PRIMLANG1  19            PRIMLANG2  20         
   PRIMLANG3  21            ROSCNT  22               NUMCHILD  23          
   HHKIDS18  24             DAUGHT918  25            SON918  26            
   NONBIOKIDS  27           MARSTAT  28              FMARSTAT  29          
   FMARIT  30               EVRMARRY  31             HPLOCALE  32          
   MANREL  33               GOSCHOL  34              VACA  35              
   HIGRADE  36-37           COMPGRD  38              DIPGED  39            
   EARNHS_Y  40-43          HISCHGRD  44-45          LSTGRADE  46-47       
   MYSCHOL_Y  48-51         HAVEDEG  52              DEGREES  53           
   EARNBA_Y  54-57          EXPSCHL  58              EXPGRADE  59-60       
   WTHPARNW  61             ONOWN  62                ONOWN18  63           
   INTACT  64               PARMARR  65              INTACT18  66          
   LVSIT14F  67             LVSIT14M  68             WOMRASDU  69          
   MOMDEGRE  70             MOMWORKD  71             MOMFSTCH  72-73       
   MOM18  74                MANRASDU  75             FOSTEREV  76          
   MNYFSTER  77-78          DURFSTER  79             AGEFSTER  80-81       
   MENARCHE  82-83          PREGNOWQ  84             MAYBPREG  85          
   EVERPREG  86             CURRPREG  87             MOSCURRP  88          
   HASBABES  89             CMLASTLB  90-93          CMLSTPRG  94-97       
   NPLACED  98              NDIED  99                NADOPTV  100          
   TOTPLACD  101            OTHERKID  102            NOTHRKID  103-104     
   RELOTHKD  105            ADPTOTKD  106            TRYADOPT  107         
   TRYEITHR  108            NBAGECAME1  109          OTHKDFOS  110         
   RELOTHKD2  111           ADPTOTKD2  112           TRYADOPT2  113        
   TRYEITHR2  114           NBAGECAME2  115          OTHKDFOS2  116        
   RELOTHKD3  117           ADPTOTKD3  118           TRYADOPT3  119        
   TRYEITHR3  120           NBAGECAME3  121          OTHKDFOS3  122        
   RELOTHKD4  123           ADPTOTKD4  124           TRYADOPT4  125        
   TRYEITHR4  126           NBAGECAME4  127          OTHKDFOS4  128        
   RELOTHKD5  129           ADPTOTKD5  130           TRYADOPT5  131        
   TRYEITHR5  132           NBAGECAME5  133          OTHKDFOS5  134        
   RELOTHKD6  135           ADPTOTKD6  136           TRYADOPT6  137        
   TRYEITHR6  138           NBAGECAME6  139          OTHKDFOS6  140        
   RELOTHKD7  141           ADPTOTKD7  142           TRYADOPT7  143        
   TRYEITHR7  144           NBAGECAME7  145          OTHKDFOS7  146        
   RELOTHKD8  147           ADPTOTKD8  148           TRYADOPT8  149        
   TRYEITHR8  150           NBAGECAME8  151          OTHKDFOS8  152        
   RELOTHKD9  153           ADPTOTKD9  154           TRYADOPT9  155        
   TRYEITHR9  156           NBAGECAME9  157          OTHKDFOS9  158        
   RELOTHKD10  159          ADPTOTKD10  160          TRYADOPT10  161       
   TRYEITHR10  162          NBAGECAME10  163         OTHKDFOS10  164       
   RELOTHKD11  165          ADPTOTKD11  166          TRYADOPT11  167       
   TRYEITHR11  168          NBAGECAME11  169         OTHKDFOS11  170       
   RELOTHKD12  171          ADPTOTKD12  172          TRYADOPT12  173       
   TRYEITHR12  174          NBAGECAME12  175         OTHKDFOS12  176       
   RELOTHKD13  177          ADPTOTKD13  178          TRYADOPT13  179       
   TRYEITHR13  180          NBAGECAME13  181         OTHKDFOS13  182       
   RELOTHKD14  183          ADPTOTKD14  184          TRYADOPT14  185       
   TRYEITHR14  186          NBAGECAME14  187         OTHKDFOS14  188       
   RELOTHKD15  189          ADPTOTKD15  190          TRYADOPT15  191       
   TRYEITHR15  192          NBAGECAME15  193         OTHKDFOS15  194       
   RELOTHKD16  195          ADPTOTKD16  196          TRYADOPT16  197       
   TRYEITHR16  198          NBAGECAME16  199         OTHKDFOS16  200       
   RELOTHKD17  201          ADPTOTKD17  202          TRYADOPT17  203       
   TRYEITHR17  204          NBAGECAME17  205         OTHKDFOS17  206       
   RELOTHKD18  207          ADPTOTKD18  208          TRYADOPT18  209       
   TRYEITHR18  210          NBAGECAME18  211         OTHKDFOS18  212       
   EVERADPT  213            SEEKADPT  214            CONTAGEM  215         
   TRYLONG  216             KNOWADPT  217            EVWNTANO  218         
   EVCONTAG  219            TURNDOWN  220            YQUITTRY  221         
   APROCESS1  222           APROCESS2  223           APROCESS3  224        
   HRDEMBRYO  225           SRCEMBRYO_1  226         SRCEMBRYO_2  227      
   SRCEMBRYO_3  228         SRCEMBRYO_4  229         SRCEMBRYO_5  230      
   TIMESMAR  231-232        HSBVERIF  233            HXAGEMAR  234-235     
   LVTOGHX  236             STRTOGHX_Y  237-240      ENGAGHX  241          
   HSBRACE1  242            HSBNRACE1  243           CHEDMARN  244         
   MARBEFHX  245            KIDSHX  246              NUMKDSHX  247-248     
   KIDLIVHX  249            CHKID18A  250            CHKID18B  251         
   WHRCHKDS1  252           WHRCHKDS2  253           WHRCHKDS3  254        
   SUPPORCH  255            BIOHUSBX  256            BIONUMHX  257         
   HXAGEMAR2  258-259       LVTOGHX2  260            STRTOGHX_Y2  261-264  
   ENGAGHX2  265            HSBRACE2  266            HSBNRACE2  267        
   CHEDMARN2  268           MARBEFHX2  269           KIDSHX2  270          
   NUMKDSHX2  271-272       KIDLIVHX2  273           CHKID18A2  274        
   CHKID18B2  275-276       WHRCHKDS5  277           WHRCHKDS6  278        
   SUPPORCH2  279           BIOHUSBX2  280           BIONUMHX2  281        
   HXAGEMAR3  282-283       LVTOGHX3  284            STRTOGHX_Y3  285-288  
   ENGAGHX3  289            HSBRACE3  290            HSBNRACE3  291        
   CHEDMARN3  292           MARBEFHX3  293           KIDSHX3  294          
   NUMKDSHX3  295           KIDLIVHX3  296           CHKID18A3  297        
   CHKID18B3  298           WHRCHKDS9  299           WHRCHKDS10  300       
   SUPPORCH3  301           BIOHUSBX3  302           BIONUMHX3  303        
   HXAGEMAR4  304-305       LVTOGHX4  306            STRTOGHX_Y4  307-310  
   ENGAGHX4  311            HSBRACE4  312            HSBNRACE4  313        
   CHEDMARN4  314           MARBEFHX4  315           KIDSHX4  316          
   NUMKDSHX4  317-318       KIDLIVHX4  319           CHKID18A4  320        
   CHKID18B4  321           WHRCHKDS13  322          WHRCHKDS14  323       
   SUPPORCH4  324           BIOHUSBX4  325           BIONUMHX4  326        
   CMMARRCH  327-330        PREVHUSB  331            WNSTRTCP_Y  332-335   
   CPHERAGE  336-337        CPHISAGE  338-339        CPENGAG1  340         
   WILLMARR  341            CURCOHRACE  342          CURCOHNRACE  343      
   CPEDUC  344              CPMARBEF  345            CPKIDS  346           
   CPNUMKDS  347-348        CPKIDLIV  349            CPKID18A  350         
   CPKID18B  351            WHRCPKDS1  352           WHRCPKDS2  353        
   WHRCPKDS3  354           SUPPORCP  355            BIOCP  356            
   BIONUMCP  357            CMSTRTHP  358-361        LIVEOTH  362          
   EVRCOHAB  363            HMOTHMEN  364-365        PREVCOHB  366         
   STRTOTHX_Y  367-370      HERAGECX  371-372        HISAGECX  373-374     
   ENGAG1CX  375            COH1RACE  376            COH1NRACE  377        
   MAREVCX  378             CXKIDS  379              BIOFCPX  380          
   BIONUMCX  381            STPTOGCX_Y  382-385      STRTOTHX_Y2  386-389  
   HERAGECX2  390-391       HISAGECX2  392-393       ENGAG1CX2  394        
   MAREVCX2  395            CXKIDS2  396             BIOFCPX2  397         
   BIONUMCX2  398           STPTOGCX_Y2  399-402     STRTOTHX_Y3  403-406  
   HERAGECX3  407-408       HISAGECX3  409-410       ENGAG1CX3  411        
   MAREVCX3  412            CXKIDS3  413             BIOFCPX3  414         
   BIONUMCX3  415           STPTOGCX_Y3  416-419     STRTOTHX_Y4  420-423  
   HERAGECX4  424-425       HISAGECX4  426-427       ENGAG1CX4  428        
   MAREVCX4  429            CXKIDS4  430             BIOFCPX4  431         
   BIONUMCX4  432           STPTOGCX_Y4  433-436     COHCHANCE  437        
   MARRCHANCE  438          PMARCOH  439             EVERSEX  440          
   RHADSEX  441             YNOSEX  442              GRFSTSX  443-444      
   SXMTONCE  445            TALKPAR1  446-447        TALKPAR2  448         
   TALKPAR3  449            TALKPAR4  450            TALKPAR5  451         
   TALKPAR6  452            TALKPAR7  453            SEDNO  454            
   SEDNOLC1  455            SEDNOLC2  456            SEDNOLC3  457         
   SEDNOLC4  458            SEDNOG  459-460          SEDNOSX  461          
   SEDBC  462               SEDBCLC1  463            SEDBCLC2  464         
   SEDBCLC3  465            SEDBCLC4  466            SEDBCG  467-468       
   SEDBCSX  469             SEDWHBC  470             SEDWHLC1  471         
   SEDWHLC2  472            SEDWHLC3  473            SEDWHLC4  474         
   SEDWHBCG  475-476        SEDWBCSX  477            SEDCOND  478          
   SEDCONLC1  479           SEDCONLC2  480           SEDCONLC3  481        
   SEDCONLC4  482           SEDCONDG  483-484        SEDCONSX  485         
   SEDSTD  486              SEDSTDLC1  487           SEDSTDLC2  488        
   SEDSTDLC3  489           SEDSTDLC4  490           SEDSTDG  491-492      
   SEDSTDSX  493            SEDHIV  494              SEDHIVLC1  495        
   SEDHIVLC2  496           SEDHIVLC3  497           SEDHIVLC4  498        
   SEDHIVG  499-500         SEDHIVSX  501            SEDABST  502          
   SEDABLC1  503            SEDABLC2  504            SEDABLC3  505         
   SEDABLC4  506            SEDABSTG  507-508        SEDABSSX  509         
   SAMEMAN  510             WHOFSTPR  511-512        KNOWFP  513-514       
   STILFPSX  515            LSTSEXFP_M  516-517      LSTSEXFP_Y  518-521   
   CMLSEXFP  522-525        CMFPLAST  526-529        FPOTHREL  530-531     
   FPEDUC  532              FSEXRACE  533            FSEXNRACE  534        
   FPRN  535-536            WHICH1ST  537            SEXAFMEN  538         
   PTSB4MAR  539-541        WHOSNC1Y  542            MATCHFP  543          
   MATCHHP  544-545         P1YRELP  546-547         CMLSEX  548-551       
   P1YLSEX_M  552-553       P1YLSEX_Y  554-557       P1YCURRP  558         
   PCURRNT  559             MATCHFP2  560            MATCHHP2  561-562     
   P1YRELP2  563-564        CMLSEX2  565-568         P1YLSEX_M2  569-570   
   P1YLSEX_Y2  571-574      P1YCURRP2  575           PCURRNT2  576         
   MATCHFP3  577            MATCHHP3  578-579        P1YRELP3  580-581     
   CMLSEX3  582-585         P1YLSEX_M3  586-587      P1YLSEX_Y3  588-591   
   P1YCURRP3  592           PCURRNT3  593            P1YOTHREL  594-595    
   P1YRAGE  596-597         P1YHSAGE  598-599        P1YRF  600-601        
   P1YFSEX_M  602-603       P1YFSEX_Y  604-607       CMFSEX  608-611       
   CMFSEXTOT  612-615       P1YEDUC  616             P1YRACE1  617         
   P1YNRACE1  618           P1YRN  619               P1YOTHREL2  620-621   
   P1YRAGE2  622-623        P1YHSAGE2  624-625       P1YRF2  626-627       
   P1YFSEX_M2  628-629      P1YFSEX_Y2  630-633      CMFSEX2  634-637      
   CMFSEXTOT2  638-641      P1YEDUC2  642            P1YRACE2  643         
   P1YNRACE2  644           P1YRN2  645-646          P1YOTHREL3  647-648   
   P1YRAGE3  649-650        P1YHSAGE3  651-652       P1YRF3  653-654       
   P1YFSEX_M3  655-656      P1YFSEX_Y3  657-660      CMFSEX3  661-664      
   CMFSEXTOT3  665-668      P1YEDUC3  669            P1YRACE3  670         
   P1YNRACE3  671           P1YRN3  672-673          CURRPRTT  674         
   CURRPRTS  675            CMCURRP1  676-679        CMCURRP2  680-683     
   CMCURRP3  684            EVERTUBS  685            ESSURE  686           
   EVERHYST  687            EVEROVRS  688            EVEROTHR  689         
   WHTOOPRC  690            ONOTFUNC  691            DFNLSTRL  692         
   ANYTUBAL  693            HYST  694                OVAREM  695           
   OTHR  696                ANYFSTER  697            ANYOPSMN  698         
   WHATOPSM  699            DFNLSTRM  700            ANYMSTER  701         
   ANYVAS  702              OTHRM  703               DATFEMOP_Y  704-707   
   PLCFEMOP  708-709        PAYRSTER1  710           PAYRSTER2  711        
   PAYRSTER3  712           RHADALL  713             HHADALL  714          
   FMEDREAS1  715           FMEDREAS2  716           FMEDREAS3  717        
   FMEDREAS4  718           FMEDREAS5  719           BCREAS  720           
   BCWHYF  721              MINCDNNR  722            DATFEMOP_Y2  723-726  
   PLCFEMOP2  727-728       PAYRSTER6  729           PAYRSTER7  730        
   PAYRSTER8  731           RHADALL2  732            HHADALL2  733         
   FMEDREAS7  734           FMEDREAS8  735           FMEDREAS9  736        
   FMEDREAS10  737          FMEDREAS11  738          BCREAS2  739          
   BCWHYF2  740             MINCDNNR2  741           DATFEMOP_Y3  742-745  
   PLCFEMOP3  746           PAYRSTER11  747          PAYRSTER12  748       
   PAYRSTER13  749          RHADALL3  750            HHADALL3  751         
   FMEDREAS13  752          FMEDREAS14  753          FMEDREAS15  754       
   FMEDREAS16  755          FMEDREAS17  756          BCREAS3  757          
   BCWHYF3  758             MINCDNNR3  759           DATFEMOP_Y4  760-763  
   PLCFEMOP4  764           PAYRSTER16  765          PAYRSTER17  766       
   PAYRSTER18  767          RHADALL4  768            HHADALL4  769         
   FMEDREAS19  770          FMEDREAS20  771          FMEDREAS21  772       
   FMEDREAS22  773          FMEDREAS23  774          BCREAS4  775          
   BCWHYF4  776             MINCDNNR4  777           OPERSAME1  778        
   OPERSAME2  779           OPERSAME3  780           OPERSAME4  781        
   OPERSAME5  782           OPERSAME6  783           DATEOPMN_Y  784-787   
   WITHIMOP  788            VASJAN4YR  789           PLACOPMN  790-791     
   PAYMSTER1  792           PAYMSTER2  793           RHADALLM  794         
   HHADALLM  795            MEDREAS1  796            MEDREAS2  797         
   BCREASM  798             BCWHYM  799              MINCDNMN  800         
   REVSTUBL  801            DATRVSTB_Y  802-805      REVSVASX  806         
   DATRVVEX_Y  807-810      TUBS  811                VASECT  812           
   RSURGSTR  813            PSURGSTR  814            ONLYTBVS  815         
   RWANTRVT  816            MANWANTT  817            RWANTREV  818         
   MANWANTR  819            POSIBLPG  820            REASIMPR  821-822     
   POSIBLMN  823            REASIMPP  824            CANHAVER  825         
   REASDIFF1  826           REASDIFF2  827           REASDIFF3  828        
   REASDIFF4  829           REASDIFF5  830           CANHAVEM  831         
   PREGNONO  832            REASNONO1  833           REASNONO2  834        
   REASNONO3  835           RSTRSTAT  836            PSTRSTAT  837         
   LASTPER  838-839         TRYPREG12  840           PILL  841             
   CONDOM  842              VASECTMY  843            DEPOPROV  844         
   WIDRAWAL  845            RHYTHM  846              SDAYCBDS  847         
   TEMPSAFE  848            PATCH  849               RING  850             
   MORNPILL  851            ECTIMESX  852-853        ECREASON1  854        
   ECREASON2  855           ECREASON3  856           ECRX  857             
   ECWHERE  858-859         ECWHEN  860              OTHRMETH01  861-862   
   OTHRMETH02  863-864      OTHRMETH03  865-866      OTHRMETH04  867-868   
   OTHRMETH05  869-870      OTHRMETH06  871-872      OTHRMETH07  873-874   
   EVIUDTYP1  875           EVIUDTYP2  876           EVIUDTYP3  877        
   NEWMETH  878             EVERUSED  879            METHDISS  880         
   METHSTOP01  881-882      METHSTOP02  883-884      METHSTOP03  885-886   
   METHSTOP04  887-888      METHSTOP05  889-890      METHSTOP06  891-892   
   METHSTOP07  893-894      METHSTOP08  895-896      METHSTOP09  897-898   
   METHSTOP10  899-900      WHENPILL  901            REASPILL01  902-903   
   REASPILL02  904-905      REASPILL03  906-907      REASPILL04  908-909   
   REASPILL05  910-911      REASPILL06  912-913      REASPILL07  914-915   
   REASPILL08  916-917      REASPILL09  918-919      STOPPILL1  920-921    
   STOPPILL2  922-923       STOPPILL3  924-925       STOPPILL4  926-927    
   STOPPILL5  928-929       STOPPILL6  930-931       WHENCOND  932         
   REASCOND01  933-934      REASCOND02  935-936      REASCOND03  937-938   
   REASCOND04  939-940      REASCOND05  941-942      REASCOND06  943-944   
   REASCOND07  945          STOPCOND1  946-947       STOPCOND2  948        
   WHENIUD  949             TYPEIUD_1  950           TYPEIUD_2  951        
   TYPEIUD_3  952           REASIUD01  953-954       REASIUD02  955-956    
   REASIUD03  957-958       REASIUD04  959-960       REASIUD05  961-962    
   REASIUD06  963-964       REASIUD07  965           REASIUD08  966        
   STOPIUD1  967-968        STOPIUD2  969-970        STOPIUD3  971-972     
   STOPIUD4  973-974        STOPIUD5  975-976        LARC10  977           
   LARCREMV  978            REMOVWHY_1  979          REMOVWHY_2  980       
   REMOVWHY_3  981          REMOVWHY_4  982          REMOVWHY_5  983       
   REMOVDIF  984            REMVDIFY_1  985          REMVDIFY_2  986       
   REMVDIFY_3  987          FIRSMETH1  988-989       FIRSMETH2  990-991    
   FIRSMETH3  992-993       FIRSMETH4  994-995       NUMFIRSM  996         
   DRUGDEV  997-998         DRUGDEV2  999            DRUGDEV3  1000        
   DRUGDEV4  1001           FIRSTIME1  1002          FIRSTIME2  1003       
   USEFSTSX  1004-1005      CMFIRSM  1006-1009       MTHFSTSX1  1010-1011  
   MTHFSTSX2  1012-1013     MTHFSTSX3  1014-1015     MTHFSTSX4  1016-1017  
   WNFSTUSE_M  1018-1019    WNFSTUSE_Y  1020-1023    CMFSTUSE  1024-1027   
   FMETHPRS  1028           FMETHPRS2  1029          FMETHPRS3  1030       
   FMETHPRS4  1031          AGEFSTUS  1032-1033      PLACGOTF  1034-1035   
   PLACGOTF2  1036-1037     PLACGOTF3  1038-1039     PLACGOTF4  1040-1041  
   USEFRSTS  1042           MTHFRSTS1  1043-1044     MTHFRSTS2  1045-1046  
   MTHFRSTS3  1047          MTHFRSTS4  1048-1049     INTR_EC3  1050        
   CMMONSX  1051-1054       MONSX  1055              CMMONSX2  1056-1059   
   MONSX2  1060             CMMONSX3  1061-1064      MONSX3  1065          
   CMMONSX4  1066-1069      MONSX4  1070             CMMONSX5  1071-1074   
   MONSX5  1075             CMMONSX6  1076-1079      MONSX6  1080          
   CMMONSX7  1081-1084      MONSX7  1085             CMMONSX8  1086-1089   
   MONSX8  1090             CMMONSX9  1091-1094      MONSX9  1095          
   CMMONSX10  1096-1099     MONSX10  1100            CMMONSX11  1101-1104  
   MONSX11  1105            CMMONSX12  1106-1109     MONSX12  1110         
   CMMONSX13  1111-1114     MONSX13  1115            CMMONSX14  1116-1119  
   MONSX14  1120            CMMONSX15  1121-1124     MONSX15  1125         
   CMMONSX16  1126-1129     MONSX16  1130            CMMONSX17  1131-1134  
   MONSX17  1135            CMMONSX18  1136-1139     MONSX18  1140         
   CMMONSX19  1141-1144     MONSX19  1145            CMMONSX20  1146-1149  
   MONSX20  1150            CMMONSX21  1151-1154     MONSX21  1155         
   CMMONSX22  1156-1159     MONSX22  1160            CMMONSX23  1161-1164  
   MONSX23  1165            CMMONSX24  1166-1169     MONSX24  1170         
   CMMONSX25  1171-1174     MONSX25  1175            CMMONSX26  1176-1179  
   MONSX26  1180            CMMONSX27  1181-1184     MONSX27  1185         
   CMMONSX28  1186-1189     MONSX28  1190            CMMONSX29  1191-1194  
   MONSX29  1195            CMMONSX30  1196-1199     MONSX30  1200         
   CMMONSX31  1201-1204     MONSX31  1205            CMMONSX32  1206-1209  
   MONSX32  1210            CMMONSX33  1211-1214     MONSX33  1215         
   CMMONSX34  1216-1219     MONSX34  1220            CMMONSX35  1221-1224  
   MONSX35  1225            CMMONSX36  1226-1229     MONSX36  1230         
   CMMONSX37  1231-1234     MONSX37  1235            CMMONSX38  1236-1239  
   MONSX38  1240            CMMONSX39  1241-1244     MONSX39  1245         
   CMMONSX40  1246-1249     MONSX40  1250            CMMONSX41  1251-1254  
   MONSX41  1255            CMMONSX42  1256-1259     MONSX42  1260         
   CMMONSX43  1261-1264     MONSX43  1265            CMMONSX44  1266-1269  
   MONSX44  1270            CMMONSX45  1271-1274     MONSX45  1275         
   CMMONSX46  1276-1279     MONSX46  1280            CMMONSX47  1281-1284  
   MONSX47  1285            CMMONSX48  1286-1289     MONSX48  1290         
   CMSTRTMC  1291-1294      CMENDMC  1295-1298       INTR_ED4A  1299       
   NUMMCMON  1300-1301      MC1MONS1  1302-1304      MC1SIMSQ  1305        
   MC1MONS2  1306-1308      MC1MONS3  1309-1311      DATBEGIN_M  1312-1313 
   DATBEGIN_Y  1314-1317    CMDATBEGIN  1318-1321    CURRMETH1  1322-1323  
   CURRMETH2  1324-1325     CURRMETH3  1326-1327     CURRMETH4  1328-1329  
   LSTMONMETH1  1330-1331   LSTMONMETH2  1332-1333   LSTMONMETH3  1334-1335
   LSTMONMETH4  1336-1337   PILL_12  1338            DIAPH_12  1339        
   IUD_12  1340             IMPLANT_12  1341         DEPO_12  1342         
   CERVC_12  1343           MPILL_12  1344           PATCH_12  1345        
   RING_12  1346            METHX1  1347-1348        METHX2  1349-1350     
   METHX3  1351-1352        METHX4  1353-1354        METHX5  1355-1356     
   METHX6  1357-1358        METHX7  1359-1360        METHX8  1361          
   METHX9  1362-1363        METHX10  1364-1365       METHX11  1366-1367    
   METHX12  1368            METHX13  1369-1370       METHX14  1371-1372    
   METHX15  1373-1374       METHX16  1375            METHX17  1376-1377    
   METHX18  1378-1379       METHX19  1380-1381       METHX20  1382-1383    
   METHX21  1384-1385       METHX22  1386-1387       METHX23  1388-1389    
   METHX24  1390            METHX25  1391-1392       METHX26  1393-1394    
   METHX27  1395-1396       METHX28  1397            METHX29  1398-1399    
   METHX30  1400-1401       METHX31  1402-1403       METHX32  1404         
   METHX33  1405-1406       METHX34  1407-1408       METHX35  1409-1410    
   METHX36  1411-1412       METHX37  1413-1414       METHX38  1415-1416    
   METHX39  1417-1418       METHX40  1419-1420       METHX41  1421-1422    
   METHX42  1423-1424       METHX43  1425-1426       METHX44  1427-1428    
   METHX45  1429-1430       METHX46  1431-1432       METHX47  1433-1434    
   METHX48  1435-1436       METHX49  1437-1438       METHX50  1439-1440    
   METHX51  1441-1442       METHX52  1443-1444       METHX53  1445-1446    
   METHX54  1447-1448       METHX55  1449-1450       METHX56  1451-1452    
   METHX57  1453-1454       METHX58  1455-1456       METHX59  1457-1458    
   METHX60  1459-1460       METHX61  1461-1462       METHX62  1463-1464    
   METHX63  1465-1466       METHX64  1467-1468       METHX65  1469-1470    
   METHX66  1471-1472       METHX67  1473-1474       METHX68  1475-1476    
   METHX69  1477-1478       METHX70  1479-1480       METHX71  1481-1482    
   METHX72  1483-1484       METHX73  1485-1486       METHX74  1487-1488    
   METHX75  1489-1490       METHX76  1491-1492       METHX77  1493-1494    
   METHX78  1495-1496       METHX79  1497-1498       METHX80  1499-1500    
   METHX81  1501-1502       METHX82  1503-1504       METHX83  1505-1506    
   METHX84  1507-1508       METHX85  1509-1510       METHX86  1511-1512    
   METHX87  1513-1514       METHX88  1515-1516       METHX89  1517-1518    
   METHX90  1519-1520       METHX91  1521-1522       METHX92  1523-1524    
   METHX93  1525-1526       METHX94  1527-1528       METHX95  1529-1530    
   METHX96  1531-1532       METHX97  1533-1534       METHX98  1535-1536    
   METHX99  1537-1538       METHX100  1539           METHX101  1540-1541   
   METHX102  1542-1543      METHX103  1544-1545      METHX104  1546        
   METHX105  1547-1548      METHX106  1549-1550      METHX107  1551-1552   
   METHX108  1553-1554      METHX109  1555-1556      METHX110  1557-1558   
   METHX111  1559-1560      METHX112  1561           METHX113  1562-1563   
   METHX114  1564-1565      METHX115  1566-1567      METHX116  1568-1569   
   METHX117  1570-1571      METHX118  1572-1573      METHX119  1574-1575   
   METHX120  1576           METHX121  1577-1578      METHX122  1579-1580   
   METHX123  1581-1582      METHX124  1583           METHX125  1584-1585   
   METHX126  1586-1587      METHX127  1588-1589      METHX128  1590        
   METHX129  1591-1592      METHX130  1593-1594      METHX131  1595-1596   
   METHX132  1597-1598      METHX133  1599-1600      METHX134  1601-1602   
   METHX135  1603-1604      METHX136  1605-1606      METHX137  1607-1608   
   METHX138  1609-1610      METHX139  1611-1612      METHX140  1613-1614   
   METHX141  1615-1616      METHX142  1617-1618      METHX143  1619-1620   
   METHX144  1621-1622      METHX145  1623-1624      METHX146  1625-1626   
   METHX147  1627-1628      METHX148  1629-1630      METHX149  1631-1632   
   METHX150  1633-1634      METHX151  1635-1636      METHX152  1637-1638   
   METHX153  1639-1640      METHX154  1641-1642      METHX155  1643-1644   
   METHX156  1645           METHX157  1646-1647      METHX158  1648-1649   
   METHX159  1650-1651      METHX160  1652-1653      METHX161  1654-1655   
   METHX162  1656-1657      METHX163  1658-1659      METHX164  1660-1661   
   METHX165  1662-1663      METHX166  1664-1665      METHX167  1666-1667   
   METHX168  1668-1669      METHX169  1670-1671      METHX170  1672-1673   
   METHX171  1674-1675      METHX172  1676-1677      METHX173  1678-1679   
   METHX174  1680-1681      METHX175  1682-1683      METHX176  1684        
   METHX177  1685-1686      METHX178  1687-1688      METHX179  1689-1690   
   METHX180  1691           METHX181  1692-1693      METHX182  1694-1695   
   METHX183  1696-1697      METHX184  1698           METHX185  1699-1700   
   METHX186  1701-1702      METHX187  1703-1704      METHX188  1705        
   METHX189  1706-1707      METHX190  1708-1709      METHX191  1710-1711   
   METHX192  1712           CMMHCALX1  1713-1716     CMMHCALX2  1717-1720  
   CMMHCALX3  1721-1724     CMMHCALX4  1725-1728     CMMHCALX5  1729-1732  
   CMMHCALX6  1733-1736     CMMHCALX7  1737-1740     CMMHCALX8  1741-1744  
   CMMHCALX9  1745-1748     CMMHCALX10  1749-1752    CMMHCALX11  1753-1756 
   CMMHCALX12  1757-1760    CMMHCALX13  1761-1764    CMMHCALX14  1765-1768 
   CMMHCALX15  1769-1772    CMMHCALX16  1773-1776    CMMHCALX17  1777-1780 
   CMMHCALX18  1781-1784    CMMHCALX19  1785-1788    CMMHCALX20  1789-1792 
   CMMHCALX21  1793-1796    CMMHCALX22  1797-1800    CMMHCALX23  1801-1804 
   CMMHCALX24  1805-1808    CMMHCALX25  1809-1812    CMMHCALX26  1813-1816 
   CMMHCALX27  1817-1820    CMMHCALX28  1821-1824    CMMHCALX29  1825-1828 
   CMMHCALX30  1829-1832    CMMHCALX31  1833-1836    CMMHCALX32  1837-1840 
   CMMHCALX33  1841-1844    CMMHCALX34  1845-1848    CMMHCALX35  1849-1852 
   CMMHCALX36  1853-1856    CMMHCALX37  1857-1860    CMMHCALX38  1861-1864 
   CMMHCALX39  1865-1868    CMMHCALX40  1869-1872    CMMHCALX41  1873-1876 
   CMMHCALX42  1877-1880    CMMHCALX43  1881-1884    CMMHCALX44  1885-1888 
   CMMHCALX45  1889-1892    CMMHCALX46  1893-1896    CMMHCALX47  1897-1900 
   CMMHCALX48  1901-1904    NUMMULTX1  1905          NUMMULTX2  1906       
   NUMMULTX3  1907          NUMMULTX4  1908          NUMMULTX5  1909       
   NUMMULTX6  1910          NUMMULTX7  1911          NUMMULTX8  1912       
   NUMMULTX9  1913          NUMMULTX10  1914         NUMMULTX11  1915      
   NUMMULTX12  1916         NUMMULTX13  1917         NUMMULTX14  1918      
   NUMMULTX15  1919         NUMMULTX16  1920         NUMMULTX17  1921      
   NUMMULTX18  1922         NUMMULTX19  1923         NUMMULTX20  1924      
   NUMMULTX21  1925         NUMMULTX22  1926         NUMMULTX23  1927      
   NUMMULTX24  1928         NUMMULTX25  1929         NUMMULTX26  1930      
   NUMMULTX27  1931         NUMMULTX28  1932         NUMMULTX29  1933      
   NUMMULTX30  1934         NUMMULTX31  1935         NUMMULTX32  1936      
   NUMMULTX33  1937         NUMMULTX34  1938         NUMMULTX35  1939      
   NUMMULTX36  1940         NUMMULTX37  1941         NUMMULTX38  1942      
   NUMMULTX39  1943         NUMMULTX40  1944         NUMMULTX41  1945      
   NUMMULTX42  1946         NUMMULTX43  1947         NUMMULTX44  1948      
   NUMMULTX45  1949         NUMMULTX46  1950         NUMMULTX47  1951      
   NUMMULTX48  1952         SALMX1  1953             SALMX2  1954          
   SALMX3  1955             SALMX4  1956             SALMX5  1957          
   SALMX6  1958             SALMX7  1959             SALMX8  1960          
   SALMX9  1961             SALMX10  1962            SALMX11  1963         
   SALMX12  1964            SALMX13  1965            SALMX14  1966         
   SALMX15  1967            SALMX16  1968            SALMX17  1969         
   SALMX18  1970            SALMX19  1971            SALMX20  1972         
   SALMX21  1973            SALMX22  1974            SALMX23  1975         
   SALMX24  1976            SALMX25  1977            SALMX26  1978         
   SALMX27  1979            SALMX28  1980            SALMX29  1981         
   SALMX30  1982            SALMX31  1983            SALMX32  1984         
   SALMX33  1985            SALMX34  1986            SALMX35  1987         
   SALMX36  1988            SALMX37  1989            SALMX38  1990         
   SALMX39  1991            SALMX40  1992            SALMX41  1993         
   SALMX42  1994            SALMX43  1995            SALMX44  1996         
   SALMX45  1997            SALMX46  1998            SALMX47  1999         
   SALMX48  2000            SAYX1  2001              SAYX2  2002           
   SAYX3  2003              SAYX4  2004              SAYX5  2005           
   SAYX6  2006              SAYX7  2007              SAYX8  2008           
   SAYX9  2009              SAYX10  2010             SAYX11  2011          
   SAYX12  2012             SAYX13  2013             SAYX14  2014          
   SAYX15  2015             SAYX16  2016             SAYX17  2017          
   SAYX18  2018             SAYX19  2019             SAYX20  2020          
   SAYX21  2021             SAYX22  2022             SAYX23  2023          
   SAYX24  2024             SAYX25  2025             SAYX26  2026          
   SAYX27  2027             SAYX28  2028             SAYX29  2029          
   SAYX30  2030             SAYX31  2031             SAYX32  2032          
   SAYX33  2033             SAYX34  2034             SAYX35  2035          
   SAYX36  2036             SAYX37  2037             SAYX38  2038          
   SAYX39  2039             SAYX40  2040             SAYX41  2041          
   SAYX42  2042             SAYX43  2043             SAYX44  2044          
   SAYX45  2045             SAYX46  2046             SAYX47  2047          
   SAYX48  2048             SIMSEQX1  2049           SIMSEQX2  2050        
   SIMSEQX3  2051           SIMSEQX4  2052           SIMSEQX5  2053        
   SIMSEQX6  2054           SIMSEQX7  2055           SIMSEQX8  2056        
   SIMSEQX9  2057           SIMSEQX10  2058          SIMSEQX11  2059       
   SIMSEQX12  2060          SIMSEQX13  2061          SIMSEQX14  2062       
   SIMSEQX15  2063          SIMSEQX16  2064          SIMSEQX17  2065       
   SIMSEQX18  2066          SIMSEQX19  2067          SIMSEQX20  2068       
   SIMSEQX21  2069          SIMSEQX22  2070          SIMSEQX23  2071       
   SIMSEQX24  2072          SIMSEQX25  2073          SIMSEQX26  2074       
   SIMSEQX27  2075          SIMSEQX28  2076          SIMSEQX29  2077       
   SIMSEQX30  2078          SIMSEQX31  2079          SIMSEQX32  2080       
   SIMSEQX33  2081          SIMSEQX34  2082          SIMSEQX35  2083       
   SIMSEQX36  2084          SIMSEQX37  2085          SIMSEQX38  2086       
   SIMSEQX39  2087          SIMSEQX40  2088          SIMSEQX41  2089       
   SIMSEQX42  2090          SIMSEQX43  2091          SIMSEQX44  2092       
   SIMSEQX45  2093          SIMSEQX46  2094          SIMSEQX47  2095       
   SIMSEQX48  2096          USELSTP  2097            WYNOLSTP  2098        
   HPLSTP  2099             LSTMTHP1  2100-2101      LSTMTHP2  2102-2103   
   LSTMTHP3  2104-2105      LSTMTHP4  2106           LPIUDTYP  2107        
   USEFSTP  2108            FSTMTHP1  2109-2110      FSTMTHP2  2111-2112   
   FSTMTHP3  2113-2114      FSTMTHP4  2115-2116      USELSTP2  2117        
   WYNOLSTP2  2118          HPLSTP2  2119            LSTMTHP5  2120-2121   
   LSTMTHP6  2122-2123      LSTMTHP7  2124-2125      LSTMTHP8  2126-2127   
   LPIUDTYP2  2128          USEFSTP2  2129           FSTMTHP5  2130-2131   
   FSTMTHP6  2132-2133      FSTMTHP7  2134-2135      FSTMTHP8  2136        
   USELSTP3  2137           WYNOLSTP3  2138          HPLSTP3  2139         
   LSTMTHP9  2140-2141      LSTMTHP10  2142-2143     LSTMTHP11  2144       
   LSTMTHP12  2145          LPIUDTYP3  2146          USEFSTP3  2147        
   FSTMTHP9  2148-2149      FSTMTHP10  2150-2151     FSTMTHP11  2152       
   FSTMTHP12  2153          WYNOTUSE  2154           HPPREGQ  2155         
   DURTRY_N  2156-2157      DURTRY_P  2158           WHYNOUSING1  2159-2160
   WHYNOUSING2  2161        WHYNOUSING3  2162        WHYNOUSING4  2163     
   WHYNOUSING5  2164        WHYNOTPG1  2165-2166     WHYNOTPG2  2167-2168  
   MAINNOUSE  2169          YUSEPILL1  2170          YUSEPILL2  2171       
   YUSEPILL3  2172          YUSEPILL4  2173          YUSEPILL5  2174       
   YUSEPILL6  2175          YUSEPILL7  2176          IUDTYPE  2177         
   YUSEIUD1  2178-2179      YUSEIUD2  2180           YUSEIUD3  2181        
   YUSEIUD4  2182           YUSEIUD5  2183           YUSEIUD6  2184        
   EKMETHOD  2185-2186      CURBCPLC  2187-2188      NUMPILLS  2189-2190   
   CURBCPAY1  2191          CURBCPAY2  2192          CURBCPAY3  2193       
   CURBCINS  2194           NOUSEINS1  2195          NOUSEINS2  2196       
   NOUSEINS3  2197          CURBCAMT  2198           NOCOST1  2199         
   NOCOST2  2200            PST4WKSX  2201-2203      PSWKCOND1  2204       
   PSWKCOND2  2205-2207     COND1BRK  2208           COND1OFF  2209        
   CONDBRFL  2210-2212      CONDOFF  2213-2215       MISSPILL  2216        
   P12MOCON  2217           PXNOFREQ  2218           BTHCON12  2219        
   MEDTST12  2220           BCCNS12  2221            STEROP12  2222        
   STCNS12  2223            EMCON12  2224            ECCNS12  2225         
   NUMMTH12  2226           PRGTST12  2227           ABORT12  2228         
   PAP12  2229              PELVIC12  2230           PRENAT12  2231        
   PARTUM12  2232           STDSVC12  2233           BARRIER1  2234-2235   
   BARRIER2  2236-2237      BARRIER3  2238-2239      BARRIER4  2240        
   BARRIER5  2241           BARRIER6  2242           NUMSVC12  2243-2244   
   NUMBCVIS  2245           BC12PLCX  2246-2247      BC12PLCX2  2248-2249  
   BC12PLCX3  2250-2251     BC12PLCX4  2252-2253     BC12PLCX5  2254       
   BC12PLCX6  2255-2256     BC12PLCX7  2257-2258     BC12PLCX8  2259-2260  
   BC12PLCX9  2261-2262     BC12PLCX10  2263-2264    BC12PLCX11  2265-2266 
   BC12PLCX12  2267-2268    BC12PLCX13  2269-2270    BC12PLCX14  2271-2272 
   BC12PLCX15  2273-2274    IDCLINIC  2275-2276      TALKPROV_1  2277      
   TALKPROV_2  2278         TALKPROV_3  2279         WHYPSTD  2280         
   WHYNOSTD  2281           BCCLARC  2282            BC12PAYX1  2283       
   BC12PAYX2  2284          BC12PAYX3  2285          BC12PAYX4  2286       
   BC12PAYX7  2287          BC12PAYX8  2288          BC12PAYX9  2289       
   BC12PAYX10  2290         BC12PAYX13  2291         BC12PAYX14  2292      
   BC12PAYX15  2293         BC12PAYX16  2294         BC12PAYX19  2295      
   BC12PAYX20  2296         BC12PAYX21  2297         BC12PAYX22  2298      
   BC12PAYX25  2299         BC12PAYX26  2300         BC12PAYX27  2301      
   BC12PAYX28  2302         BC12PAYX31  2303         BC12PAYX32  2304      
   BC12PAYX33  2305         BC12PAYX34  2306         BC12PAYX37  2307      
   BC12PAYX38  2308         BC12PAYX39  2309         BC12PAYX40  2310      
   BC12PAYX43  2311         BC12PAYX44  2312         BC12PAYX45  2313      
   BC12PAYX46  2314         BC12PAYX49  2315         BC12PAYX50  2316      
   BC12PAYX51  2317         BC12PAYX52  2318         BC12PAYX55  2319      
   BC12PAYX56  2320         BC12PAYX57  2321         BC12PAYX58  2322      
   BC12PAYX61  2323         BC12PAYX62  2324         BC12PAYX63  2325      
   BC12PAYX64  2326         BC12PAYX67  2327         BC12PAYX68  2328      
   BC12PAYX69  2329         BC12PAYX70  2330         BC12PAYX73  2331      
   BC12PAYX74  2332         BC12PAYX75  2333         BC12PAYX76  2334      
   BC12PAYX79  2335         BC12PAYX80  2336         BC12PAYX81  2337      
   BC12PAYX82  2338         BC12PAYX85  2339         BC12PAYX86  2340      
   BC12PAYX87  2341         BC12PAYX88  2342         REGCAR12_F_01  2343   
   REGCAR12_F_02  2344      REGCAR12_F_03  2345      REGCAR12_F_04  2346   
   REGCAR12_F_05  2347      REGCAR12_F_06  2348      REGCAR12_F_07  2349   
   REGCAR12_F_08  2350      REGCAR12_F_09  2351      REGCAR12_F_10  2352   
   REGCAR12_F_11  2353      REGCAR12_F_12  2354      REGCAR12_F_13  2355   
   REGCAR12_F_14  2356      REGCAR12_F_15  2357      EVERFPC  2358         
   KNDMDHLP01  2359-2360    KNDMDHLP02  2361-2362    KNDMDHLP03  2363-2364 
   KNDMDHLP04  2365-2366    KNDMDHLP05  2367-2368    KNDMDHLP06  2369-2370 
   KNDMDHLP07  2371-2372    KNDMDHLP08  2373-2374    KNDMDHLP09  2375-2376 
   LASTPAP  2377            MREASPAP  2378           AGEFPAP  2379-2380    
   AGEFPAP2  2381           ABNPAP3  2382            INTPAP  2383-2384     
   PELWPAP  2385            LASTPEL  2386            MREASPEL  2387        
   AGEFPEL  2388-2389       AGEPEL2  2390            INTPEL  2391-2392     
   EVHPVTST  2393           HPVWPAP  2394            LASTHPV  2395         
   MREASHPV  2396           AGEFHPV  2397-2398       AGEHPV2  2399         
   INTHPV  2400-2401        ASKSMOKE  2402           ASKPREG  2403         
   ASKFOLIC  2404           TALKDM  2405             PROVRESP  2406        
   PROVSAYBC  2407          PROVPREBC  2408          PROVINFOBC  2409      
   RWANT  2410              PROBWANT  2411           PWANT  2412           
   JINTEND  2413            JSUREINT  2414           JINTENDN  2415-2416   
   JEXPECTL  2417-2418      JEXPECTS  2419           JINTNEXT  2420        
   INTEND  2421             SUREINT  2422            INTENDN  2423-2424    
   EXPECTL  2425-2426       EXPECTS  2427            INTNEXT  2428         
   HLPPRG  2429             HOWMANYR  2430           SEEKWHO1  2431        
   SEEKWHO2  2432           TYPALLPG1  2433          TYPALLPG2  2434       
   TYPALLPG3  2435          TYPALLPG4  2436          TYPALLPG5  2437       
   TYPALLPG6  2438          WHOTEST  2439            OVUL12M  2440         
   WHARTIN  2441            INSEM12M  2442           OTMEDHEP1  2443       
   OTMEDHEP2  2444          OTMEDHEP3  2445          OTMEDHEP4  2446       
   INSCOVPG  2447           FSTHLPPG_M  2448-2449    FSTHLPPG_Y  2450-2453 
   CMPGVIS1  2454-2457      TRYLONG2  2458-2459      UNIT_TRYLONG  2460    
   HLPPGNOW  2461           RCNTPGH_M  2462-2463     RCNTPGH_Y  2464-2467  
   CMPGVISL  2468-2471      NUMVSTPG  2472-2473      PRGVISIT  2474-2476   
   HLPMC  2477              TYPALLMC1  2478          TYPALLMC2  2479       
   TYPALLMC3  2480          TYPALLMC4  2481          TYPALLMC5  2482       
   MISCNUM  2483            INFRTPRB1  2484          INFRTPRB2  2485       
   INFRTPRB3  2486          INFRTPRB4  2487          INFRTPRB5  2488       
   DUCHFREQ  2489           PID  2490                PIDSYMPT  2491        
   PIDTX  2492-2493         LSTPIDTX_M  2494-2495    LSTPIDTX_Y  2496-2499 
   CMPIDLST  2500-2503      DIABETES  2504           GESTDIAB  2505        
   UF  2506                 UFSONO  2507             UFCURR  2508          
   UFDIAGNOS  2509          UFLIMIT  2510            UFTREAT_1  2511       
   UFTREAT_2  2512          UFTREAT_3  2513          UFTREAT_4  2514       
   UFTREAT_5  2515          UFTREAT_6  2516          UFTREAT_7  2517       
   UFTREAT_8  2518          ENDO  2519               ENDOCURR  2520        
   ENDODIAG  2521           ENDOLIM  2522            ENDOTREAT_1  2523-2524
   ENDOTREAT_2  2525        ENDOTREAT_3  2526        ENDOTREAT_4  2527     
   ENDOTREAT_5  2528        ENDOTREAT_6  2529        ENDOTREAT_7  2530     
   ENDOTREAT_8  2531        OVUPROB  2532            PCOS  2533            
   PCOSSYMP_1  2534         PCOSSYMP_2  2535         PCOSSYMP_3  2536      
   PCOSSYMP_4  2537         PCOSSYMP_5  2538         PCOSSYMP_6  2539      
   DEAF  2540               BLIND  2541              DIFDECIDE  2542       
   DIFWALK  2543            DIFDRESS  2544           DIFOUT  2545          
   EVRCANCER  2546          AGECANCER  2547-2548     PRECANCER  2549       
   MAMMOG  2550             AGEMAMM1  2551-2552      REASMAMM1  2553       
   CLINEXAM  2554           FAMHYST  2555            MOMRISK70  2556       
   MOMRISK40  2557          ALCORISK  2558           BFEEDRISK  2559       
   DONBLOOD  2560           HIVTEST  2561            NOHIVTST  2562-2563   
   WHENHIV_M  2564-2565     WHENHIV_Y  2566-2569     CMHIVTST  2570-2573   
   HIVTSTYR  2574           HIVRESULT  2575          WHYNOGET  2576-2577   
   PLCHIV  2578-2579        RHHIVT1  2580            RHHIVT21  2581        
   RHHIVT22  2582           HIVTST  2583-2584        PREPHIV  2585         
   TALKDOCT  2586           AIDSTALK01  2587-2588    AIDSTALK02  2589-2590 
   AIDSTALK03  2591-2592    AIDSTALK04  2593-2594    AIDSTALK05  2595-2596 
   AIDSTALK06  2597-2598    AIDSTALK07  2599-2600    AIDSTALK08  2601-2602 
   AIDSTALK09  2603-2604    AIDSTALK10  2605-2606    AIDSTALK11  2607-2608 
   RETROVIR  2609           PREGHIV  2610            EVERVACC  2611        
   HPVSHOT1  2612-2613      HPVSEX1  2614            VACCPROB  2615        
   BLDPRESS  2616           HIGHBP  2617             BPMEDS  2618          
   NUTRINFO_1  2619         NUTRINFO_2  2620         NUTRINFO_3  2621      
   NUTRINFO_4  2622         NUTRINFO_5  2623         USUALCAR  2624        
   USLPLACE  2625-2626      USL12MOS  2627           CURRCOV  2628         
   COVERHOW01  2629-2630    COVERHOW02  2631         COVERHOW03  2632      
   PARINSUR  2633           INS_EXCH  2634           INS_PREM  2635        
   COVER12  2636            NUMNOCOV  2637-2638      SAMEADD  2639         
   BRNOUT  2640             RELRAISD  2641-2642      ATTND14  2643         
   RELCURR  2644-2645       RELTRAD  2646            FUNDAM1  2647         
   FUNDAM2  2648            FUNDAM3  2649            FUNDAM4  2650         
   RELDLIFE  2651           ATTNDNOW  2652           MILSVC  2653          
   WRK12MOS  2654-2655      FPT12MOS  2656           DOLASTWK1  2657       
   DOLASTWK2  2658          DOLASTWK3  2659          DOLASTWK4  2660       
   DOLASTWK5  2661          DOLASTWK6  2662          RWRKST  2663          
   WORKP12  2664            RPAYJOB  2665            RNUMJOB  2666         
   RFTPTX  2667             REARNTY  2668            SPLSTWK1  2669        
   SPLSTWK2  2670           SPLSTWK3  2671           SPLSTWK4  2672        
   SPLSTWK5  2673           SPWRKST  2674            SPPAYJOB  2675        
   SPNUMJOB  2676           SPFTPTX  2677            SPEARNTY  2678        
   SAMESEX  2679            CHSUPPOR  2680           REACTSLF  2681        
   CHBOTHER  2682           ACASILANG  2683          GENHEALT  2684        
   BMICAT  2685             DRWEIGH  2686            TELLWGHT  2687        
   WGHTSCRN  2688           ENGSPEAK  2689           CASIBIRTH  2690-2691  
   CASILOSS  2692-2693      CASIABOR  2694-2695      CASIADOP  2696        
   NOBEDYR  2697            STAYREL  2698            EVSUSPEN  2699        
   GRADSUSP  2700-2701      SMK100  2702             AGESMK  2703-2704     
   SMOKE12  2705            SMKSTOP  2706            DRINK12  2707         
   UNIT30D  2708            DRINK30D  2709-2710      DRINKDAY  2711-2712   
   BINGE30  2713-2714       DRNKMOST  2715-2716      BINGE12  2717         
   POT12  2718              COC12  2719              CRACK12  2720         
   CRYSTMTH12  2721         INJECT12  2722           VAGSEX  2723          
   AGEVAGR  2724-2725       CONDVAG  2726            WHYCONDL  2727        
   GETORALM  2728           GIVORALM  2729           CONDFELL  2730        
   ANYORAL  2731            TIMING  2732             ANALSEX  2733         
   CONDANAL  2734           OPPSEXANY  2735          OPPSEXGEN  2736       
   CONDSEXL  2737           OPPLIFENUM  2738-2740    OPPYEARNUM  2741-2743 
   VAGNUM12  2744-2746      ORALNUM12  2747-2749     ANALNUM12  2750-2752  
   BISEXPRT  2753           NONMONOG  2754           NNONMONOG1  2755      
   NNONMONOG2  2756         NNONMONOG3  2757         MALSHT12  2758        
   PROSTFRQ  2759           JOHNFREQ  2760           HIVMAL12  2761        
   GIVORALF  2762           GETORALF  2763           FEMSEX  2764          
   SAMESEXANY  2765         SAMLIFENUM  2766-2768    SAMYEARNUM  2769-2771 
   SAMESEX1  2772-2773      FSAMEREL  2774-2775      MFLASTP  2776         
   DATEAPP  2777            ATTRACT  2778            ORIENT_A  2779        
   ORIENT_B  2780           CONFCONC  2781           TIMALON  2782         
   RISKCHEK1  2783          RISKCHEK2  2784          RISKCHEK3  2785       
   RISKCHEK4  2786          CHLAMTST  2787           STDOTHR12  2788       
   STDTRT12  2789           GON  2790                CHLAM  2791           
   HERPES  2792             GENWARTS  2793           SYPHILIS  2794        
   EVRINJECT  2795          EVRSHARE  2796           EARNTYPE  2797        
   EARN  2798-2799          EARNDK1  2800            EARNDK2  2801         
   EARNDK3  2802            EARNDK4  2803            TOINCWMY  2804        
   TOTINC  2805-2806        FMINCDK1  2807           FMINCDK2  2808        
   FMINCDK3  2809           FMINCDK4  2810           FMINCDK5  2811        
   PUBASST  2812            PUBASTYP1  2813          FOODSTMP  2814        
   WIC  2815                HLPTRANS  2816           HLPCHLDC  2817        
   HLPJOB  2818             FREEFOOD  2819           HUNGRY  2820          
   MED_COST  2821           AGER  2822-2823          FMARITAL  2824        
   RMARITAL  2825           EDUCAT  2826-2827        HIEDUC  2828-2829     
   HISPANIC  2830           RACE  2831               HISPRACE2  2832       
   NUMKDHH  2833            NUMFMHH  2834            HHFAMTYP  2835        
   HHPARTYP  2836           NCHILDHH  2837           HHKIDTYP  2838        
   CSPBBHH  2839            CSPBSHH  2840            CSPOKDHH  2841        
   INTCTFAM  2842           PARAGE14  2843           EDUCMOM  2844-2845    
   AGEMOMB1  2846-2847      AGER_I  2848             FMARITAL_I  2849      
   RMARITAL_I  2850         EDUCAT_I  2851           HIEDUC_I  2852        
   HISPANIC_I  2853         RACE_I  2854             HISPRACE2_I  2855     
   NUMKDHH_I  2856          NUMFMHH_I  2857          HHFAMTYP_I  2858      
   HHPARTYP_I  2859         NCHILDHH_I  2860         HHKIDTYP_I  2861      
   CSPBBHH_I  2862          CSPBSHH_I  2863          CSPOKDHH_I  2864      
   INTCTFAM_I  2865         PARAGE14_I  2866         EDUCMOM_I  2867       
   AGEMOMB1_I  2868         RCURPREG  2869           PREGNUM  2870-2871    
   COMPREG  2872-2873       LOSSNUM  2874-2875       ABORTION  2876        
   LBPREGS  2877-2878       PARITY  2879-2880        BIRTHS5  2881         
   BIRTH1  2882             OUTCOM01  2883           OUTCOM02  2884        
   OUTCOM03  2885           OUTCOM04  2886           OUTCOM05  2887        
   OUTCOM06  2888           OUTCOM07  2889           OUTCOM08  2890        
   OUTCOM09  2891           OUTCOM10  2892           OUTCOM11  2893        
   OUTCOM12  2894           OUTCOM13  2895           OUTCOM14  2896        
   DATEND01  2897-2900      DATEND02  2901-2904      DATEND03  2905-2908   
   DATEND04  2909-2912      DATEND05  2913-2916      DATEND06  2917-2920   
   DATEND07  2921-2924      DATEND08  2925-2928      DATEND09  2929-2932   
   DATEND10  2933-2936      DATEND11  2937-2940      DATEND12  2941-2944   
   DATEND13  2945-2948      DATEND14  2949-2952      AGEPRG01  2953-2954   
   AGEPRG02  2955-2956      AGEPRG03  2957-2958      AGEPRG04  2959-2960   
   AGEPRG05  2961-2962      AGEPRG06  2963-2964      AGEPRG07  2965-2966   
   AGEPRG08  2967-2968      AGEPRG09  2969-2970      AGEPRG10  2971-2972   
   AGEPRG11  2973-2974      AGEPRG12  2975-2976      AGEPRG13  2977-2978   
   AGEPRG14  2979-2980      DATCON01  2981-2984      DATCON02  2985-2988   
   DATCON03  2989-2992      DATCON04  2993-2996      DATCON05  2997-3000   
   DATCON06  3001-3004      DATCON07  3005-3008      DATCON08  3009-3012   
   DATCON09  3013-3016      DATCON10  3017-3020      DATCON11  3021-3024   
   DATCON12  3025-3028      DATCON13  3029-3032      DATCON14  3033-3036   
   AGECON01  3037-3038      AGECON02  3039-3040      AGECON03  3041-3042   
   AGECON04  3043-3044      AGECON05  3045-3046      AGECON06  3047-3048   
   AGECON07  3049-3050      AGECON08  3051-3052      AGECON09  3053-3054   
   AGECON10  3055-3056      AGECON11  3057-3058      AGECON12  3059-3060   
   AGECON13  3061-3062      AGECON14  3063-3064      MAROUT01  3065        
   MAROUT02  3066           MAROUT03  3067           MAROUT04  3068        
   MAROUT05  3069           MAROUT06  3070           MAROUT07  3071        
   MAROUT08  3072           MAROUT09  3073           MAROUT10  3074        
   MAROUT11  3075           MAROUT12  3076           MAROUT13  3077        
   MAROUT14  3078           RMAROUT01  3079          RMAROUT02  3080       
   RMAROUT03  3081          RMAROUT04  3082          RMAROUT05  3083       
   RMAROUT06  3084          RMAROUT07  3085          RMAROUT08  3086       
   RMAROUT09  3087          RMAROUT10  3088          RMAROUT11  3089       
   RMAROUT12  3090          RMAROUT13  3091          RMAROUT14  3092       
   MARCON01  3093           MARCON02  3094           MARCON03  3095        
   MARCON04  3096           MARCON05  3097           MARCON06  3098        
   MARCON07  3099           MARCON08  3100           MARCON09  3101        
   MARCON10  3102           MARCON11  3103           MARCON12  3104        
   MARCON13  3105           MARCON14  3106           RMARCON01  3107       
   RMARCON02  3108          RMARCON03  3109          RMARCON04  3110       
   RMARCON05  3111          RMARCON06  3112          RMARCON07  3113       
   RMARCON08  3114          RMARCON09  3115          RMARCON10  3116       
   RMARCON11  3117          RMARCON12  3118          RMARCON13  3119       
   RMARCON14  3120          CEBOW  3121-3122         CEBOWC  3123          
   DATBABY1  3124-3127      AGEBABY1  3128-3129      LASTPREG  3130        
   PREG_INT1  3131          PREG_INT2  3132          PREG_INT3  3133       
   PREG_INT4  3134          RCURPREG_I  3135         PREGNUM_I  3136       
   COMPREG_I  3137          LOSSNUM_I  3138          ABORTION_I  3139      
   LBPREGS_I  3140          PARITY_I  3141           BIRTHS5_I  3142       
   OUTCOM01_I  3143         OUTCOM02_I  3144         OUTCOM03_I  3145      
   OUTCOM04_I  3146         OUTCOM05_I  3147         OUTCOM06_I  3148      
   OUTCOM07_I  3149         OUTCOM08_I  3150         OUTCOM09_I  3151      
   OUTCOM10_I  3152         OUTCOM11_I  3153         OUTCOM12_I  3154      
   OUTCOM13_I  3155         OUTCOM14_I  3156         DATEND01_I  3157      
   DATEND02_I  3158         DATEND03_I  3159         DATEND04_I  3160      
   DATEND05_I  3161         DATEND06_I  3162         DATEND07_I  3163      
   DATEND08_I  3164         DATEND09_I  3165         DATEND10_I  3166      
   DATEND11_I  3167         DATEND12_I  3168         DATEND13_I  3169      
   DATEND14_I  3170         AGEPRG01_I  3171         AGEPRG02_I  3172      
   AGEPRG03_I  3173         AGEPRG04_I  3174         AGEPRG05_I  3175      
   AGEPRG06_I  3176         AGEPRG07_I  3177         AGEPRG08_I  3178      
   AGEPRG09_I  3179         AGEPRG10_I  3180         AGEPRG11_I  3181      
   AGEPRG12_I  3182         AGEPRG13_I  3183         AGEPRG14_I  3184      
   DATCON01_I  3185         DATCON02_I  3186         DATCON03_I  3187      
   DATCON04_I  3188         DATCON05_I  3189         DATCON06_I  3190      
   DATCON07_I  3191         DATCON08_I  3192         DATCON09_I  3193      
   DATCON10_I  3194         DATCON11_I  3195         DATCON12_I  3196      
   DATCON13_I  3197         DATCON14_I  3198         AGECON01_I  3199      
   AGECON02_I  3200         AGECON03_I  3201         AGECON04_I  3202      
   AGECON05_I  3203         AGECON06_I  3204         AGECON07_I  3205      
   AGECON08_I  3206         AGECON09_I  3207         AGECON10_I  3208      
   AGECON11_I  3209         AGECON12_I  3210         AGECON13_I  3211      
   AGECON14_I  3212         MAROUT01_I  3213         MAROUT02_I  3214      
   MAROUT03_I  3215         MAROUT04_I  3216         MAROUT05_I  3217      
   MAROUT06_I  3218         MAROUT07_I  3219         MAROUT08_I  3220      
   MAROUT09_I  3221         MAROUT10_I  3222         MAROUT11_I  3223      
   MAROUT12_I  3224         MAROUT13_I  3225         MAROUT14_I  3226      
   RMAROUT01_I  3227        RMAROUT02_I  3228        RMAROUT03_I  3229     
   RMAROUT04_I  3230        RMAROUT05_I  3231        RMAROUT06_I  3232     
   RMAROUT07_I  3233        RMAROUT08_I  3234        RMAROUT09_I  3235     
   RMAROUT10_I  3236        RMAROUT11_I  3237        RMAROUT12_I  3238     
   RMAROUT13_I  3239        RMAROUT14_I  3240        MARCON01_I  3241      
   MARCON02_I  3242         MARCON03_I  3243         MARCON04_I  3244      
   MARCON05_I  3245         MARCON06_I  3246         MARCON07_I  3247      
   MARCON08_I  3248         MARCON09_I  3249         MARCON10_I  3250      
   MARCON11_I  3251         MARCON12_I  3252         MARCON13_I  3253      
   MARCON14_I  3254         RMARCON01_I  3255        RMARCON02_I  3256     
   RMARCON03_I  3257        RMARCON04_I  3258        RMARCON05_I  3259     
   RMARCON06_I  3260        RMARCON07_I  3261        RMARCON08_I  3262     
   RMARCON09_I  3263        RMARCON10_I  3264        RMARCON11_I  3265     
   RMARCON12_I  3266        RMARCON13_I  3267        RMARCON14_I  3268     
   CEBOW_I  3269            CEBOWC_I  3270           DATBABY1_I  3271      
   AGEBABY1_I  3272         FMARNO  3273             CSPBIOKD  3274        
   MARDAT01  3275-3278      MARDAT02  3279-3282      FMAR2AGE  3283-3284   
   MARDAT03  3285-3288      FMAR3AGE  3289-3290      MARDAT04  3291-3294   
   FMAR4AGE  3295-3296      MARDIS01  3297-3300      MARDIS02  3301-3304   
   MARDIS03  3305-3308      MARDIS04  3309-3312      MAREND01  3313        
   MAREND02  3314           MAREND03  3315           MAREND04  3316        
   FMAR1AGE  3317-3318      AGEDISS1  3319-3320      AGEDD1  3321-3322     
   MAR1DISS  3323           DD1REMAR  3324           MAR1BIR1  3325-3327   
   MAR1CON1  3328-3330      CON1MAR1  3331-3333      B1PREMAR  3334        
   COHEVER  3335            EVMARCOH  3336           PMARRNO  3337-3338    
   NONMARR  3339-3340       TIMESCOH  3341-3342      COHAB1  3343-3346     
   COHSTAT  3347            COHOUT  3348             COH1DUR  3349         
   HADSEX  3350             SEXONCE  3351            SEXEVER  3352         
   VRY1STAG  3353-3354      SEX1AGE  3355-3356       VRY1STSX  3357-3360   
   DATESEX1  3361-3364      SEXMAR  3365             SEX1FOR  3366         
   SEXUNION  3367           SEXOUT  3368             FPDUR  3369-3371      
   PARTS1YR  3372           LSEXDATE  3373-3376      LSEXPREG  3377        
   SEX3MO  3378             NUMP3MOS  3379           LSEXRAGE  3380-3381   
   PARTDUR1  3382-3384      PARTDUR2  3385-3387      PARTDUR3  3388-3390   
   RELATP1  3391            RELATP2  3392            RELATP3  3393         
   LIFPRTNR  3394-3395      FMARNO_I  3396           CSPBIOKD_I  3397      
   MARDAT01_I  3398         MARDAT02_I  3399         MARDAT03_I  3400      
   MARDAT04_I  3401         MARDIS01_I  3402         MARDIS02_I  3403      
   MARDIS03_I  3404         MARDIS04_I  3405         MAREND01_I  3406      
   MAREND02_I  3407         MAREND03_I  3408         MAREND04_I  3409      
   FMAR1AGE_I  3410         AGEDISS1_I  3411         AGEDD1_I  3412        
   MAR1DISS_I  3413         DD1REMAR_I  3414         MAR1BIR1_I  3415      
   MAR1CON1_I  3416         CON1MAR1_I  3417         B1PREMAR_I  3418      
   COHEVER_I  3419          EVMARCOH_I  3420         PMARRNO_I  3421       
   NONMARR_I  3422          TIMESCOH_I  3423         COHAB1_I  3424        
   COHSTAT_I  3425          COHOUT_I  3426           COH1DUR_I  3427       
   HADSEX_I  3428           SEXEVER_I  3429          VRY1STAG_I  3430      
   SEX1AGE_I  3431          VRY1STSX_I  3432         DATESEX1_I  3433      
   SEXONCE_I  3434          SEXMAR_I  3435           SEX1FOR_I  3436       
   SEXUNION_I  3437         SEXOUT_I  3438           FPDUR_I  3439         
   PARTS1YR_I  3440         LSEXDATE_I  3441         SEXP3MO_I  3442       
   NUMP3MOS_I  3443         LSEXRAGE_I  3444         PARTDUR1_I  3445      
   PARTDUR2_I  3446         PARTDUR3_I  3447         RELATP1_I  3448       
   RELATP2_I  3449          RELATP3_I  3450          LIFPRTNR_I  3451      
   STRLOPER  3452           FECUND  3453             ANYBC36  3454         
   NOSEX36  3455            INFERT  3456             ANYBC12  3457         
   ANYMTHD  3458            NOSEX12  3459-3460       SEXP3MO  3461         
   CONSTAT1  3462-3463      CONSTAT2  3464-3465      CONSTAT3  3466-3467   
   CONSTAT4  3468-3469      PILLR  3470              CONDOMR  3471         
   SEX1MTHD1  3472-3473     SEX1MTHD2  3474-3475     SEX1MTHD3  3476-3477  
   SEX1MTHD4  3478-3479     MTHUSE12  3480-3481      METH12M1  3482-3483   
   METH12M2  3484-3485      METH12M3  3486-3487      METH12M4  3488        
   MTHUSE3  3489-3490       METH3M1  3491-3492       METH3M2  3493-3494    
   METH3M3  3495-3496       METH3M4  3497            FMETHOD1  3498-3499   
   FMETHOD2  3500-3501      FMETHOD3  3502-3503      FMETHOD4  3504-3505   
   DATEUSE1  3506-3510      OLDWP01  3511            OLDWP02  3512         
   OLDWP03  3513            OLDWP04  3514            OLDWP05  3515         
   OLDWP06  3516            OLDWP07  3517            OLDWP08  3518         
   OLDWP09  3519            OLDWP10  3520            OLDWP11  3521         
   OLDWP12  3522            OLDWP13  3523            OLDWP14  3524         
   OLDWR01  3525            OLDWR02  3526            OLDWR03  3527         
   OLDWR04  3528            OLDWR05  3529            OLDWR06  3530         
   OLDWR07  3531            OLDWR08  3532            OLDWR09  3533         
   OLDWR10  3534            OLDWR11  3535            OLDWR12  3536         
   OLDWR13  3537            OLDWR14  3538            WANTRP01  3539        
   WANTRP02  3540           WANTRP03  3541           WANTRP04  3542        
   WANTRP05  3543           WANTRP06  3544           WANTRP07  3545        
   WANTRP08  3546           WANTRP09  3547           WANTRP10  3548        
   WANTRP11  3549           WANTRP12  3550           WANTRP13  3551        
   WANTRP14  3552           WANTP01  3553            WANTP02  3554         
   WANTP03  3555            WANTP04  3556            WANTP05  3557         
   WANTP06  3558            WANTP07  3559            WANTP08  3560         
   WANTP09  3561            WANTP10  3562            WANTP11  3563         
   WANTP12  3564            WANTP13  3565            WANTP14  3566         
   NWWANTRP01  3567         NWWANTRP02  3568         NWWANTRP03  3569      
   NWWANTRP04  3570         NWWANTRP05  3571         NWWANTRP06  3572      
   NWWANTRP07  3573         NWWANTRP08  3574         NWWANTRP09  3575      
   NWWANTRP10  3576         NWWANTRP11  3577         NWWANTRP12  3578      
   NWWANTRP13  3579         NWWANTRP14  3580         WANTP5  3581          
   STRLOPER_I  3582         FECUND_I  3583           INFERT_I  3584        
   ANYMTHD_I  3585          NOSEX12_I  3586          SEX3MO_I  3587        
   CONSTAT1_I  3588         CONSTAT2_I  3589         CONSTAT3_I  3590      
   CONSTAT4_I  3591         PILLR_I  3592            CONDOMR_I  3593       
   SEX1MTHD1_I  3594        SEX1MTHD2_I  3595        SEX1MTHD3_I  3596     
   SEX1MTHD4_I  3597        MTHUSE12_I  3598         METH12M1_I  3599      
   METH12M2_I  3600         METH12M3_I  3601         METH12M4_I  3602      
   MTHUSE3_I  3603          METH3M1_I  3604          METH3M2_I  3605       
   METH3M3_I  3606          METH3M4_I  3607          FMETHOD1_I  3608      
   FMETHOD2_I  3609         FMETHOD3_I  3610         FMETHOD4_I  3611      
   DATEUSE1_I  3612         OLDWP01_I  3613          OLDWP02_I  3614       
   OLDWP03_I  3615          OLDWP04_I  3616          OLDWP05_I  3617       
   OLDWP06_I  3618          OLDWP07_I  3619          OLDWP08_I  3620       
   OLDWP09_I  3621          OLDWP10_I  3622          OLDWP11_I  3623       
   OLDWP12_I  3624          OLDWP13_I  3625          OLDWP14_I  3626       
   OLDWR01_I  3627          OLDWR02_I  3628          OLDWR03_I  3629       
   OLDWR04_I  3630          OLDWR05_I  3631          OLDWR06_I  3632       
   OLDWR07_I  3633          OLDWR08_I  3634          OLDWR09_I  3635       
   OLDWR10_I  3636          OLDWR11_I  3637          OLDWR12_I  3638       
   OLDWR13_I  3639          OLDWR14_I  3640          WANTRP01_I  3641      
   WANTRP02_I  3642         WANTRP03_I  3643         WANTRP04_I  3644      
   WANTRP05_I  3645         WANTRP06_I  3646         WANTRP07_I  3647      
   WANTRP08_I  3648         WANTRP09_I  3649         WANTRP10_I  3650      
   WANTRP11_I  3651         WANTRP12_I  3652         WANTRP13_I  3653      
   WANTRP14_I  3654         WANTP01_I  3655          WANTP02_I  3656       
   WANTP03_I  3657          WANTP04_I  3658          WANTP05_I  3659       
   WANTP06_I  3660          WANTP07_I  3661          WANTP08_I  3662       
   WANTP09_I  3663          WANTP10_I  3664          WANTP11_I  3665       
   WANTP12_I  3666          WANTP13_I  3667          WANTP14_I  3668       
   NWWANTRP01_I  3669       NWWANTRP02_I  3670       NWWANTRP03_I  3671    
   NWWANTRP04_I  3672       NWWANTRP05_I  3673       NWWANTRP06_I  3674    
   NWWANTRP07_I  3675       NWWANTRP08_I  3676       NWWANTRP09_I  3677    
   NWWANTRP10_I  3678       NWWANTRP11_I  3679       NWWANTRP12_I  3680    
   NWWANTRP13_I  3681       NWWANTRP14_I  3682       WANTP5_I  3683        
   FPTIT12  3684            FPTITMED  3685           FPTITSTE  3686-3687   
   FPTITBC  3688-3689       FPTITCHK  3690-3691      FPTITCBC  3692-3693   
   FPTITCST  3694-3695      FPTITEC  3696-3697       FPTITCEC  3698-3699   
   FPTITPRE  3700-3701      FPTITABO  3702-3703      FPTITPAP  3704-3705   
   FPTITPEL  3706-3707      FPTITPRN  3708-3709      FPTITPPR  3710-3711   
   FPTITSTD  3712-3713      FPREGFP  3714            FPREGMED  3715        
   FPTIT12_I  3716          FPTITMED_I  3717         FPTITSTE_I  3718      
   FPTITBC_I  3719          FPTITCHK_I  3720         FPTITCBC_I  3721      
   FPTITCST_I  3722         FPTITEC_I  3723          FPTITCEC_I  3724      
   FPTITPRE_I  3725         FPTITABO_I  3726         FPTITPAP_I  3727      
   FPTITPEL_I  3728         FPTITPRN_I  3729         FPTITPPR_I  3730      
   FPTITSTD_I  3731         FPREGFP_I  3732          FPREGMED_I  3733      
   INTENT  3734             ADDEXP  3735-3737        INTENT_I  3738        
   ADDEXP_I  3739           ANYPRGHP  3740           ANYMSCHP  3741        
   INFEVER  3742            OVULATE  3743            TUBES  3744           
   INFERTR  3745            INFERTH  3746            ADVICE  3747          
   INSEM  3748              INVITRO  3749            ENDOMET  3750         
   FIBROIDS  3751           PIDTREAT  3752           EVHIVTST  3753        
   FPTITHIV  3754-3755      ANYPRGHP_I  3756         ANYMSCHP_I  3757      
   INFEVER_I  3758          OVULATE_I  3759          TUBES_I  3760         
   INFERTR_I  3761          INFERTH_I  3762          ADVICE_I  3763        
   INSEM_I  3764            INVITRO_I  3765          ENDOMET_I  3766       
   FIBROIDS_I  3767         PIDTREAT_I  3768         EVHIVTST_I  3769      
   FPTITHIV_I  3770         CURR_INS  3771           METRO  3772           
   RELIGION  3773           LABORFOR  3774           CURR_INS_I  3775      
   RELIGION_I  3776         LABORFOR_I  3777         POVERTY  3778-3780    
   TOTINCR  3781-3782       PUBASSIS  3783           POVERTY_I  3784       
   TOTINCR_I  3785          PUBASSIS_I  3786         WGT2017_2019  3787-3802
   SECU  3803               SEST  3804-3806          CMINTVW  3807-3810    
   CMLSTYR  3811-3814       CMJAN3YR  3815-3818      CMJAN4YR  3819-3822   
   CMJAN5YR  3823-3826      QUARTER  3827-3828       PHASE  3829           
   INTVWYEAR  3830-3833     INTVLNGTH  3834-3839   ;                       

* SAS LABEL STATEMENT;

LABEL
   CASEID = "Respondent ID number"
   RSCRNINF = "Whether R was also the screener informant"
   RSCRAGE = "R's age as reported in screener"
   RSCRHISP = "R's Hispanic origin as reported in screener"
   RSCRRACE = "R's race as reported in screener"
   AGE_A = "AA-1 R's age at interview"
   AGE_R = "R's age at interview"
   AGESCRN = "R's age at screener"
   HISP = "AC-1 R is of Hispanic/Latina origin"
   HISPGRP = "AC-2 Hispanic/Latina origin -- group"
   PRIMLANG1 = "AC-6 Language spoken in home - 1st mention"
   PRIMLANG2 = "AC-6 Language spoken in home - 2nd mention"
   PRIMLANG3 = "AC-6 Language spoken in home - 3rd mention"
   ROSCNT = "Number of HH members based on HH roster"
   NUMCHILD = "Number of children in HH < 13 yrs with RELAR = codes 3 to 8"
   HHKIDS18 = "Number of children in HH < 19 yrs with RELAR = codes 3 to 10"
   DAUGHT918 = "Number of daughters in HH 9-18 yrs with RELAR = 3 or 5"
   SON918 = "Number of sons in HH 9-18 yrs with RELAR = 3 or 5"
   NONBIOKIDS = "Number of children in HH < 19 yrs with RELAR = codes 4 to 10"
   MARSTAT = "AD-7b R's marital status"
   FMARSTAT = "AD-7c R's formal marital status"
   FMARIT = "Formal marital status at time of interview"
   EVRMARRY = "Whether R was ever married"
   HPLOCALE = "AD-8 Where H/P currently lives, if not on HH roster"
   MANREL = "Man in HH is R's husband or cohabiting partner"
   GOSCHOL = "AF-1 R currently enrolled in regular school"
   VACA = "AF-2 R on vacation from school"
   HIGRADE = "AF-3 R's current grade in school or highest grade/year attended"
   COMPGRD = "AF-4 R completed current grade or highest grade/year attended"
   DIPGED = "AF-6 Which - high school diploma, GED, or both - does R have?"
   EARNHS_Y = "AF-7 Year high school diploma obtained"
   HISCHGRD = "AF-8 R not in sch, no dipl-last grade of elem/jr/hi sch attended"
   LSTGRADE = "Highest grade of elem/junior/high school R attended"
   MYSCHOL_Y = "AF-9 Year R last attended elem/jr/hi sch-R not in sch/no dipl"
   HAVEDEG = "AF-10 R has college or university degree"
   DEGREES = "AF-11 R's highest college or university degree received"
   EARNBA_Y = "AF-12 Year R received her Bachelor's degree"
   EXPSCHL = "AF-13 Expect to go back to school"
   EXPGRADE = "AF-14 Highest grade/degree expect to complete"
   WTHPARNW = "R is living with parents or parent figures - based on HH roster"
   ONOWN = "AG-0a Did R ever live away from parents/guardians before age 18"
   ONOWN18 = "Whether R lived on own before age 18"
   INTACT = "AG-1 R always lived with both biological/adoptive parents"
   PARMARR = "AG-2 R's biological parents married at R's birth"
   INTACT18 = "Whether R lived in intact family from birth to age 18"
   LVSIT14F = "AG-3 Female parent/parent-figure at age 14 - fam not intact thru 18"
   LVSIT14M = "AG-4 Male parent/parent-figure at age 14 - fam not intact thru 18"
   WOMRASDU = "AG-5 Woman who raised R during teens - fam not intact thru 18"
   MOMDEGRE = "AG-6 Highest level of education female parent (figure) completed"
   MOMWORKD = "AG-7 Female parent (figure) worked full/parttime when R was 5-15"
   MOMFSTCH = "AG-9 Age of female parent (figure) at her first birth"
   MOM18 = "AG-10 Estimated age of female parent (figure) at her first birth"
   MANRASDU = "AG-11 Man who raised R during teens - fam not intact thru 18"
   FOSTEREV = "AG-13 R ever lived in a foster home"
   MNYFSTER = "AG-14 Number of foster homes R lived in"
   DURFSTER = "AG-15 Total time spent in foster care"
   AGEFSTER = "AG-16 Age last left foster care"
   MENARCHE = "BA-1 Age at first menstrual period"
   PREGNOWQ = "BA-2 Whether R is currently pregnant"
   MAYBPREG = "BA-3 Whether R is probably pregnant now"
   EVERPREG = "Whether R has ever been pregnant"
   CURRPREG = "Whether R is currently pregnant"
   MOSCURRP = "Number of Months Currently Pregnant"
   HASBABES = "Whether R has had any live births"
   CMLASTLB = "Year of R's most recent live birth"
   CMLSTPRG = "Year of R's most recent completed pregnancy"
   NPLACED = "# of R's bio children R placed for adoption (based on BPA)"
   NDIED = "# of R's bio children that died shortly after birth (BDS)"
   NADOPTV = "# of R's bio children living with adoptive families (based on BG-5 WHERENOW)"
   TOTPLACD = "# of R's bio children she (or someone) placed for adoption"
   OTHERKID = "BJ-1 Have any nonbiological children lived with R"
   NOTHRKID = "BJ-2 Number of nonbiological children who lived with R"
   RELOTHKD = "BJ-5 Relationship of nonbiological child to R - 1st child"
   ADPTOTKD = "BJ-6 R legally adopted nonbiological child - 1st child"
   TRYADOPT = "BJ-7a R trying to adopt nonbiological child - 1st child"
   TRYEITHR = "BJ-7b R trying to adopt/become guardian of nonbio child-1st child"
   NBAGECAME1 = "Age when 1st nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 1st child"
   RELOTHKD2 = "BJ-5 Relationship of nonbiological child to R - 2nd child"
   ADPTOTKD2 = "BJ-6 R legally adopted nonbiological child - 2nd child"
   TRYADOPT2 = "BJ-7a R trying to adopt nonbiological child - 2nd child"
   TRYEITHR2 = "BJ-7b R trying to adopt/become guardian of nonbio child - 2nd child"
   NBAGECAME2 = "Age when 2nd nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS2 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 2nd child"
   RELOTHKD3 = "BJ-5 Relationship of nonbiological child to R - 3rd child"
   ADPTOTKD3 = "BJ-6 R legally adopted nonbiological child - 3rd child"
   TRYADOPT3 = "BJ-7a R trying to adopt nonbiological child - 3rd child"
   TRYEITHR3 = "BJ-7b R trying to adopt/become guardian of nonbio child - 3rd child"
   NBAGECAME3 = "Age when 3rd nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS3 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 3rd child"
   RELOTHKD4 = "BJ-5 Relationship of nonbiological child to R - 4th child"
   ADPTOTKD4 = "BJ-6 R legally adopted nonbiological child - 4th child"
   TRYADOPT4 = "BJ-7a R trying to adopt nonbiological child - 4th child"
   TRYEITHR4 = "BJ-7b R trying to adopt/become guardian of nonbio child - 4th child"
   NBAGECAME4 = "Age when 4th nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS4 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 4th child"
   RELOTHKD5 = "BJ- 5 Relationship of nonbiological child to R - 5th child"
   ADPTOTKD5 = "BJ-6 R legally adopted nonbiological child - 5th child"
   TRYADOPT5 = "BJ-7a R trying to adopt nonbiological child - 5th child"
   TRYEITHR5 = "BJ-7b R trying to adopt/become guardian of nonbio child - 5th child"
   NBAGECAME5 = "Age when 5th nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS5 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 5th child"
   RELOTHKD6 = "BJ-5 Relationship of nonbiological child to R - 6th child"
   ADPTOTKD6 = "BJ-6 R legally adopted nonbiological child - 6th child"
   TRYADOPT6 = "BJ-7a R trying to adopt nonbiological child - 6th child"
   TRYEITHR6 = "BJ-7b R trying to adopt/become guardian of nonbio child - 6th child"
   NBAGECAME6 = "Age when 6th nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS6 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 6th child"
   RELOTHKD7 = "BJ-5 Relationship of nonbiological child to R - 7th child"
   ADPTOTKD7 = "BJ-6 R legally adopted nonbiological child - 7th child"
   TRYADOPT7 = "BJ-7a R trying to adopt nonbiological child - 7th child"
   TRYEITHR7 = "BJ-7b R trying to adopt/become guardian of nonbio child -7th child"
   NBAGECAME7 = "Age when 7th nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS7 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs -7th child"
   RELOTHKD8 = "BJ-5 Relationship of nonbiological child to R - 8th child"
   ADPTOTKD8 = "BJ-6 R legally adopted nonbiological child - 8th child"
   TRYADOPT8 = "BJ-7a R trying to adopt nonbiological child - 8th child"
   TRYEITHR8 = "BJ-7b R trying to adopt/become guardian of nonbio child - 8th child"
   NBAGECAME8 = "Age when 8th nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS8 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 8th child"
   RELOTHKD9 = "BJ-5 Relationship of nonbiological child to R - 9th child"
   ADPTOTKD9 = "BJ-6 R legally adopted nonbiological child - 9th child"
   TRYADOPT9 = "BJ-7a R trying to adopt nonbiological child - 9th child"
   TRYEITHR9 = "BJ-7b R trying to adopt/become guardian of nonbio child 9th child"
   NBAGECAME9 = "Age when 9th nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS9 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 9th child"
   RELOTHKD10 = "BJ-5 Relationship of nonbiological child to R - 10th child"
   ADPTOTKD10 = "BJ-6 R legally adopted nonbiological child - 10th child"
   TRYADOPT10 = "BJ-7a R trying to adopt nonbiological child - 10th child"
   TRYEITHR10 = "BJ-7b R trying to adopt/become guardian of nonbio child - 10th child"
   NBAGECAME10 = "Age when 10th nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS10 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 10th child"
   RELOTHKD11 = "BJ-5 Relationship of nonbiological child to R - 11th child"
   ADPTOTKD11 = "BJ-6 R legally adopted nonbiological child - 11th child"
   TRYADOPT11 = "BJ-7a R trying to adopt nonbiological child - 11th child"
   TRYEITHR11 = "BJ-7b R trying to adopt/become guardian of nonbio child - 11th child"
   NBAGECAME11 = "Age when 11th nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS11 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 11th child"
   RELOTHKD12 = "BJ-5 Relationship of nonbiological child to R - 12th child"
   ADPTOTKD12 = "BJ-6 R legally adopted nonbiological child - 12th child"
   TRYADOPT12 = "BJ-7a R trying to adopt nonbiological child - 12th child"
   TRYEITHR12 = "BJ-7b R trying to adopt/become guardian of nonbio child - 12th child"
   NBAGECAME12 = "Age when 12th nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS12 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 12th child"
   RELOTHKD13 = "BJ-5 Relationship of nonbiological child to R - 13th child"
   ADPTOTKD13 = "BJ-6 R legally adopted nonbiological child - 13th child"
   TRYADOPT13 = "BJ-7a R trying to adopt nonbiological child - 13th child"
   TRYEITHR13 = "BJ-7b R trying to adopt/become guardian of nonbio child - 13th child"
   NBAGECAME13 = "Age when 13th nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS13 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 13th child"
   RELOTHKD14 = "BJ-5 Relationship of nonbiological child to R - 14th child"
   ADPTOTKD14 = "BJ-6 R legally adopted nonbiological child - 14th child"
   TRYADOPT14 = "BJ-7a R trying to adopt nonbiological child - 14th child"
   TRYEITHR14 = "BJ-7b R trying to adopt/become guardian of nonbio child - 14th child"
   NBAGECAME14 = "Age when 14th nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS14 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 14th child"
   RELOTHKD15 = "BJ-5 Relationship of nonbiological child to R - 15th child"
   ADPTOTKD15 = "BJ-6 R legally adopted nonbiological child - 15th child"
   TRYADOPT15 = "BJ-7a R trying to adopt nonbiological child - 15th child"
   TRYEITHR15 = "BJ-7b R trying to adopt/become guardian of nonbio child - 15th child"
   NBAGECAME15 = "Age when 15th nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS15 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 15th child"
   RELOTHKD16 = "BJ-5 Relationship of nonbiological child to R - 16th child"
   ADPTOTKD16 = "BJ-6 R legally adopted nonbiological child - 16th child"
   TRYADOPT16 = "BJ-7a R trying to adopt nonbiological child - 16th child"
   TRYEITHR16 = "BJ-7b R trying to adopt/become guardian of nonbio child - 16th child"
   NBAGECAME16 = "Age when 16th nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS16 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 16th child"
   RELOTHKD17 = "BJ-5 Relationship of nonbiological child to R - 17th child"
   ADPTOTKD17 = "BJ-6 R legally adopted nonbiological child - 17th child"
   TRYADOPT17 = "BJ-7a R trying to adopt nonbiological child - 17th child"
   TRYEITHR17 = "BJ-7b R trying to adopt/become guardian of nonbio child - 17th child"
   NBAGECAME17 = "Age when 17th nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS17 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 17th child"
   RELOTHKD18 = "BJ-5 Relationship of nonbiological child to R - 18th child"
   ADPTOTKD18 = "BJ-6 R legally adopted nonbiological child - 18th child"
   TRYADOPT18 = "BJ-7a R trying to adopt nonbiological child - 18th child"
   TRYEITHR18 = "BJ-7b R trying to adopt/become guardian of nonbio child - 18th child"
   NBAGECAME18 = "Age when 18th nonbio child came to live with R (computed in post-processing)"
   OTHKDFOS18 = "BJ-10 Child was foster/related child placed w/R by Soc Svcs - 18th child"
   EVERADPT = "R's experience with adoption (based on BJ series)"
   SEEKADPT = "BK-1 R is currently seeking to adopt"
   CONTAGEM = "BK-2 R has taken specific steps to adopt"
   TRYLONG = "BK-3 How long R has been seeking to adopt"
   KNOWADPT = "BK-4 R is seeking to adopt a child she knows"
   EVWNTANO = "BL-1 R has ever considered adopting a child"
   EVCONTAG = "BL-2 R has ever taken specific steps to adopt a child"
   TURNDOWN = "BL-3 R was turned down for/decided not to pursue adoption"
   YQUITTRY = "BL-4 R decided not to pursue adoption because of adoption process or own situation"
   APROCESS1 = "BL-5 If adoption process, reason R stopped seeking to adopt - 1st mention"
   APROCESS2 = "BL-5 If adoption process, reason R stopped seeking to adopt - 2nd mention"
   APROCESS3 = "BL-5 If adoption process, reason R stopped seeking to adopt - 3rd mention"
   HRDEMBRYO = "BL-6 R has heard of frozen embryo donation or adoption"
   SRCEMBRYO_1 = "BL-7 Where R heard about embryo adoption or donation - 1st mention"
   SRCEMBRYO_2 = "BL-7 Where R heard about embryo adoption or donation - 2nd mention"
   SRCEMBRYO_3 = "BL-7 Where R heard about embryo adoption or donation - 3rd mention"
   SRCEMBRYO_4 = "BL-7 Where R heard about embryo adoption or donation - 4th mention"
   SRCEMBRYO_5 = "BL-7 Where R heard about embryo adoption or donation - 5th mention"
   TIMESMAR = "CA-1 Number of Times R Has Been Married"
   HSBVERIF = "CA-2b Verify Current Husband's Name"
   HXAGEMAR = "CB-3 Husband's Age At Marriage-1st Husband"
   LVTOGHX = "CB-5 Cohabited Before Marriage-1st Husband"
   STRTOGHX_Y = "CB-6y Year Began 1st Cohabitation-1st Husband"
   ENGAGHX = "CB-7 Engaged When R Began Premarital Cohabitation-1st Husband"
   HSBRACE1 = "RACE-recode-like variable for 1st husband"
   HSBNRACE1 = "HISPRACE2-recode-like variable for 1st husband"
   CHEDMARN = "CB-11 Highest Level of Education-1st Husband"
   MARBEFHX = "CB-12 Was He Married Before-1st Husband"
   KIDSHX = "CB-13 Did He Have Kids From Prior Relationship-1st Husband"
   NUMKDSHX = "CB-14 Number of Kids From Prior Relationships-1st Husband"
   KIDLIVHX = "CB-15 Any Kids From Prior Relationships Live w/R & 1st Husband"
   CHKID18A = "CB-16a Is This Child 18 or Younger - Curr (if 1st) Husband"
   CHKID18B = "CB-16b # of These Children 18 or Younger - Curr (if 1st) Husband"
   WHRCHKDS1 = "CB-17 Where Current (if 1st) Husband's Child(ren) Lives Now-1st Mention"
   WHRCHKDS2 = "CB-17 Where Current (if 1st) Husband's Child(ren) Lives Now-2nd Mention"
   WHRCHKDS3 = "CB-17 Where Current (if 1st) Husband's Child(ren) Lives Now-3rd Mention"
   SUPPORCH = "CB-18 Does Current (if 1st) Husband Financially Support Kids <=18 Elsewhere"
   BIOHUSBX = "CB-18b Did R have bio kids with this husband- 1st Husband"
   BIONUMHX = "CB-18c Number of bio kids R and husband had together-1st Husband"
   HXAGEMAR2 = "CB-3 Husband's Age At Marriage-2nd Husband"
   LVTOGHX2 = "CB-5 Cohabited Before Marriage-2nd Husband"
   STRTOGHX_Y2 = "CB-6y Year Began 1st Cohabitation-2nd Husband"
   ENGAGHX2 = "CB-7 Engaged When R Began Premarital Cohabitation-2nd Husband"
   HSBRACE2 = "RACE-recode-like variable for 2nd husband"
   HSBNRACE2 = "HISPRACE2-recode-like variable for 2nd husband"
   CHEDMARN2 = "CB-11 Highest Level of Education-2nd Husband"
   MARBEFHX2 = "CB-12 Was He Married Before-2nd Husband"
   KIDSHX2 = "CB-13 Did He Have Kids From Prior Relationship-2nd Husband"
   NUMKDSHX2 = "CB-14 Number of Kids From Prior Relationships-2nd Husband"
   KIDLIVHX2 = "CB-15 Any Kids From Prior Relationships Live w/R & 2nd Husband"
   CHKID18A2 = "CB-16a Is This Child 18 or Younger - Curr (if 2nd) Husband"
   CHKID18B2 = "CB-16b # of These Children 18 or Younger - Curr (if 2nd) Husband"
   WHRCHKDS5 = "CB-17 Where Current (if 2nd) Husband's Child(ren) Lives Now-1st Mention"
   WHRCHKDS6 = "CB-17 Where Current (if 2nd) Husband's Child(ren) Lives Now-2nd Mention"
   SUPPORCH2 = "CB-18 Does Current (if 2nd) Husband Financially Support Kids <=18 Elsewhere"
   BIOHUSBX2 = "CB-18b Did R have bio kids with this husband-2nd Husband"
   BIONUMHX2 = "CB-18c Number of bio kids R and husband had together-2nd Husband"
   HXAGEMAR3 = "CB-3 Husband's Age At Marriage-3rd Husband"
   LVTOGHX3 = "CB-5 Cohabited Before Marriage-3rd Husband"
   STRTOGHX_Y3 = "CB-6y Year Began 1st Cohabitation-3rd Husband"
   ENGAGHX3 = "CB-7 Engaged When R Began Premarital Cohabitation-3rd Husband"
   HSBRACE3 = "RACE-recode-like variable for 3rd husband"
   HSBNRACE3 = "HISPRACE2-recode-like variable for 3rd husband"
   CHEDMARN3 = "CB-11 Highest Level of Education-3rd Husband"
   MARBEFHX3 = "CB-12 Was He Married Before-3rd Husband"
   KIDSHX3 = "CB-13 Did He Have Kids From Prior Relationship-3rd Husband"
   NUMKDSHX3 = "CB-14 Number of Kids From Prior Relationships-3rd Husband"
   KIDLIVHX3 = "CB-15 Any Kids From Prior Relationships Live w/R & 3rd Husband"
   CHKID18A3 = "CB-16a Is This Child 18 or Younger - Curr (if 3rd) Husband"
   CHKID18B3 = "CB-16b # of These Children 18 or Younger - Curr (if 3rd) Husband"
   WHRCHKDS9 = "CB-17 Where Current (if 3rd) Husband's Child(ren) Lives Now-1st Mention"
   WHRCHKDS10 = "CB-17 Where Current (if 3rd) Husband's Child(ren) Lives Now-2nd Mention"
   SUPPORCH3 = "CB-18 Does Current (if 3rd) Husband Financially Support Kids <=18 Elsewhere"
   BIOHUSBX3 = "CB-18b Did R have bio kids with this husband-3rd Husband"
   BIONUMHX3 = "CB-18c Number of bio kids R and  husband had together-3rd Husband"
   HXAGEMAR4 = "CB-3 Husband's Age At Marriage-4th Husband"
   LVTOGHX4 = "CB-5 Cohabited Before Marriage-4th Husband"
   STRTOGHX_Y4 = "CB-6y Year Began 1st Cohabitation-4th Husband"
   ENGAGHX4 = "CB-7 Engaged When R Began Premarital Cohabitation-4th Husband"
   HSBRACE4 = "RACE-recode-like variable for 4th husband"
   HSBNRACE4 = "HISPRACE2-recode-like variable for 4th husband"
   CHEDMARN4 = "CB-11 Highest Level of Education-4th Husband"
   MARBEFHX4 = "CB-12 Was He Married Before-4th Husband"
   KIDSHX4 = "CB-13 Did He Have Kids From Prior Relationship-4th Husband"
   NUMKDSHX4 = "CB-14 Number of Kids From Prior Relationships-4th Husband"
   KIDLIVHX4 = "CB-15 Any Kids From Prior Relationships Live w/R & 4th Husband"
   CHKID18A4 = "CB-16a Is This Child 18 or Younger - Curr (if 4th) Husband"
   CHKID18B4 = "CB-16b # of These Children 18 or Younger - Curr (if 4th) Husband"
   WHRCHKDS13 = "CB-17 Where Current (if 4th) Husband's Child(ren) Lives Now-1st Mention"
   WHRCHKDS14 = "CB-17 Where Current (if 4th) Husband's Child(ren) Lives Now-2nd Mention"
   SUPPORCH4 = "CB-18 Does Current (if 4th) Husband Financially Support Kids <=18 Elsewhere"
   BIOHUSBX4 = "CB-18b Did R have bio kids with this husband-4th Husband"
   BIONUMHX4 = "CB-18c Number of bio kids R and  husband had together-4th Husband"
   CMMARRCH = "Year of Marriage to Current Husband"
   PREVHUSB = "Number of Previous Husbands (incl. separated)"
   WNSTRTCP_Y = "CC-2y Year Began Cohabitation - Current Partner"
   CPHERAGE = "CC-3 R's Age When Began Cohabiting w/Current Partner"
   CPHISAGE = "CC-4 Partner's Age When Began Cohabiting - Current Partner"
   CPENGAG1 = "CC-6 Engaged When Began Cohabiting-Current Cohabiting Partner"
   WILLMARR = "CC-7 R Thinks She Will Marry-Curr Cohabiting Partner"
   CURCOHRACE = "RACE-recode-like variable for current cohab partner"
   CURCOHNRACE = "HISPRACE2-recode-like variable for current cohab partner"
   CPEDUC = "CC-11 Level of Education-Current Cohabiting Partner"
   CPMARBEF = "CC-12 Was He Married Before - Current Cohabiting Partner"
   CPKIDS = "CC-13 Kids From Prior Relationships-Current Cohabiting Partner"
   CPNUMKDS = "CC-14 # of Kids From Prior Relationships-Curr Cohab Partner"
   CPKIDLIV = "CC-15 Kids From Prior Relationships Live w/R"
   CPKID18A = "CC-16a Is This Child 18 or Younger-Curr Cohabiting Partner"
   CPKID18B = "CC-16b # of His Children 18 or Younger-Curr Cohabiting Partner"
   WHRCPKDS1 = "CC-17 Where Curr Cohab s Child(ren) Lives Now-1st Mention"
   WHRCPKDS2 = "CC-17 Where Curr Cohab s Child(ren) Lives Now-2nd Mention"
   WHRCPKDS3 = "CC-17 Where Curr Cohab s Child(ren) Lives Now-3rd Mention"
   SUPPORCP = "CC-18 Does Curr Cohab Financially Support Kids <=18 Elsewhere"
   BIOCP = "CC-19 Did R have bio kids with current partner?"
   BIONUMCP = "CC-20 Number of bio kids R and current partner have together"
   CMSTRTHP = "Year R began living with current husband or cohab partner"
   LIVEOTH = "CD-1 Ever Cohabited With Any (Other) Man"
   EVRCOHAB = "Whether R Has Ever Lived With a Non Marital Partner"
   HMOTHMEN = "CD-2 Number of Other Men R Has Cohabited With"
   PREVCOHB = "Number of former cohabiting partners"
   STRTOTHX_Y = "CD-4y Year Began Cohabitation - 1st former partner"
   HERAGECX = "CD-5 R's Age When Began Cohabiting w/ 1st Former Partner"
   HISAGECX = "CD-6 Partner's Age When Began Cohabiting - 1st Former Partner"
   ENGAG1CX = "CD-8 Engaged When Began Cohabiting-1st former cohab partner"
   COH1RACE = "RACE-recode-like variable for 1st former cohabiting partner"
   COH1NRACE = "HISPRACE2-recode-like variable for 1st former cohabiting partner"
   MAREVCX = "CD-12 Was He Married Before-1st former cohabiting partner"
   CXKIDS = "CD-13 Any Kids From Prior Relationhips-1st former cohab partner"
   BIOFCPX = "CD-13b Did R have bio kids with this cohab partner-1st former cohab partner"
   BIONUMCX = "CD-13c Number of bio kids R and cohab partner have together-1st former cohab partner"
   STPTOGCX_Y = "CD-14y Year Stopped Cohabiting-1st former cohabiting partner"
   STRTOTHX_Y2 = "CD-4y Year Began Cohabitation - 2nd former partner"
   HERAGECX2 = "CD-5 R's Age When Began Cohabiting w/ 2nd Former Partner"
   HISAGECX2 = "CD-6 Partner's Age When Began Cohabiting - 2nd Former Partner"
   ENGAG1CX2 = "CD-8 Engaged When Began Cohabiting-2nd former cohab partner"
   MAREVCX2 = "CD-12 Was He Married Before-2nd former cohabiting partner"
   CXKIDS2 = "CD-13 Any Kids From Prior Relationhips-2nd former cohab partner"
   BIOFCPX2 = "CD-13b Did R have bio kids with this cohab partner-2nd former cohab partner"
   BIONUMCX2 = "CD-13c Number of bio kids R and cohab partner have together-2nd former cohab partner"
   STPTOGCX_Y2 = "CD-14y Year Stopped Cohabiting-2nd former cohabiting partner"
   STRTOTHX_Y3 = "CD-4y Year Began Cohabitation - 3rd former partner"
   HERAGECX3 = "CD-5 R's Age When Began Cohabiting w/ 3rd Former Partner"
   HISAGECX3 = "CD-6 Partner's Age When Began Cohabiting - 3rd Former Partner"
   ENGAG1CX3 = "CD-8 Engaged When Began Cohabiting-3rd former cohab partner"
   MAREVCX3 = "CD-12 Was He Married Before-3rd former cohabiting partner"
   CXKIDS3 = "CD-13 Any Kids From Prior Relationships-3rd former cohab partner"
   BIOFCPX3 = "CD-13b Did R have bio kids with this cohab partner-3rd former cohab partner"
   BIONUMCX3 = "CD-13c Number of bio kids R and cohab partner have together-3rd former cohab partner"
   STPTOGCX_Y3 = "CD-14y Year Stopped Cohabiting-3rd former cohabiting partner"
   STRTOTHX_Y4 = "CD-4y Year Began Cohabitation - 4th former partner"
   HERAGECX4 = "CD-5 R's Age When Began Cohabiting w/ 4th Former Partner"
   HISAGECX4 = "CD-6 Partner's Age When Began Cohabiting - 4th Former Partner"
   ENGAG1CX4 = "CD-8 Engaged When Began Cohabiting-4th former cohab partner"
   MAREVCX4 = "CD-12 Was He Married Before-4th former cohabiting partner"
   CXKIDS4 = "CD-13 Any Kids From Prior Relationhips-4th former cohab partner"
   BIOFCPX4 = "CD-13b Did R have bio kids with this cohab partner-4th former cohab partner"
   BIONUMCX4 = "CD-13c Number of bio kids R and cohab partner have together-4th former cohab partner"
   STPTOGCX_Y4 = "CD-14y Year Stopped Cohabiting-4th former cohabiting partner"
   COHCHANCE = "CD-15 R thinks she will cohabit in the future"
   MARRCHANCE = "CD-16 R thinks she will (re)marry in the future"
   PMARCOH = "CD-17 R thinks she will live with a future husband before marriage"
   EVERSEX = "CE-1 Ever Had Sexual Intercourse with a Man"
   RHADSEX = "Whether R Has Ever Had Sex (Vaginal Intercourse)"
   YNOSEX = "CE-2 Main Reason R Has Not Had Sex With a Man"
   GRFSTSX = "CE-8 Grade R Was in at First Sexual Intercourse"
   SXMTONCE = "CE-9 Has R Had Sexual Intercourse More Than Once"
   TALKPAR1 = "CF-1 Sex Ed Topics R Has Discussed with Parents -1st Mention"
   TALKPAR2 = "CF-1 Sex Ed Topics R Has Discussed with Parents -2nd Mention"
   TALKPAR3 = "CF-1 Sex Ed Topics R Has Discussed with Parents -3rd Mention"
   TALKPAR4 = "CF-1 Sex Ed Topics R Has Discussed with Parents -4th Mention"
   TALKPAR5 = "CF-1 Sex Ed Topics R Has Discussed with Parents -5th Mention"
   TALKPAR6 = "CF-1 Sex Ed Topics R Has Discussed with Parents -6th Mention"
   TALKPAR7 = "CF-1 Sex Ed Topics R Has Discussed with Parents -7th Mention"
   SEDNO = "CF-2 Formal Sex Ed Before 18: How to Say No to Sex"
   SEDNOLC1 = "CF-2a Place where received instructions on how to say no to sex-1st Mention"
   SEDNOLC2 = "CF-2a Place where received instructions on how to say no to sex-2nd Mention"
   SEDNOLC3 = "CF-2a Place where received instructions on how to say no to sex-3rd Mention"
   SEDNOLC4 = "CF-2a Place where received instructions on how to say no to sex-4th Mention"
   SEDNOG = "CF-3 Grade R Received Sex Ed on How to Say No to Sex"
   SEDNOSX = "CF-4 Rec d Sex Ed on How to Say No to Sex Before/After 1st Sex"
   SEDBC = "CF-5 Formal Sex Ed Before 18: Methods of Birth Control"
   SEDBCLC1 = "CF-5a Place where received instruction about methods of birth control-1st Mention"
   SEDBCLC2 = "CF-5a Place where received instruction about methods of birth control-2nd Mention"
   SEDBCLC3 = "CF-5a Place where received instruction about methods of birth control-3rd Mention"
   SEDBCLC4 = "CF-5a Place where received instruction about methods of birth control-4th Mention"
   SEDBCG = "CF-6 R's Grade When Received Instruction on Birth Control"
   SEDBCSX = "CF-7 Rec d Sex Ed on Birth Control Methods Before/After 1st Sex"
   SEDWHBC = "CF-8 Formal Sex Ed Before 18: Where to Get Birth Control"
   SEDWHLC1 = "CF-8A Place where received instructions on where to get birth control-1st Mention"
   SEDWHLC2 = "CF-8A Place where received instructions on where to get birth control-2nd Mention"
   SEDWHLC3 = "CF-8A Place where received instructions on where to get birth control-3rd Mention"
   SEDWHLC4 = "CF-8A Place where received instructions on where to get birth control-4th Mention"
   SEDWHBCG = "CF-9 R's Grade When Received Instruction on Where to Get Birth Control"
   SEDWBCSX = "CF-10 R rec d Instruction-Where to Get Birth Control before/after 1st sex"
   SEDCOND = "CF-11 Formal Sex Ed Before 18: How to Use a Condom"
   SEDCONLC1 = "CF-11a Place where received instructions on how to use a condom-1st Mention"
   SEDCONLC2 = "CF-11a Place where received instructions on how to use a condom-2nd Mention"
   SEDCONLC3 = "CF-11a Place where received instructions on how to use a condom-3rd Mention"
   SEDCONLC4 = "CF-11a Place where received instructions on how to use a condom-4th Mention"
   SEDCONDG = "CF-12 R's Grade When Received Instruction on How to Use a Condom"
   SEDCONSX = "CF-13 R rec d Instruction-How to Use a Condom before/after 1st sex"
   SEDSTD = "CF-14 Formal Sex Ed Before 18: STD"
   SEDSTDLC1 = "CF-14a Place where received instructions on sexually transmitted diseases-1st Mention"
   SEDSTDLC2 = "CF-14a Place where received instructions on sexually transmitted diseases-2nd Mention"
   SEDSTDLC3 = "CF-14a Place where received instructions on sexually transmitted diseases-3rd Mention"
   SEDSTDLC4 = "CF-14a Place where received instructions on sexually transmitted diseases-4th Mention"
   SEDSTDG = "CF-15 Grade R 1st Received Sex Ed on STDs"
   SEDSTDSX = "CF-16 Received Sex Ed on STD before/after 1st sex"
   SEDHIV = "CF-17 Formal Sex Ed Before 18: HIV/AIDS"
   SEDHIVLC1 = "CF-17a Place where received instructions on how to prevent HIV/AIDS-1st Mention"
   SEDHIVLC2 = "CF-17a Place where received instructions on how to prevent HIV/AIDS-2nd Mention"
   SEDHIVLC3 = "CF-17a Place where received instructions on how to prevent HIV/AIDS-3rd Mention"
   SEDHIVLC4 = "CF-17a Place where received instructions on how to prevent HIV/AIDS-4th Mention"
   SEDHIVG = "CF-18 Grade R 1st Received Sex Ed on HIV/AIDS"
   SEDHIVSX = "CF-19 Received Sex Ed on HIV/AIDS before/after 1st sex"
   SEDABST = "CF-20 Formal Sex Ed Before 18: Waiting Until Marriage"
   SEDABLC1 = "CF-20a Place where received instruction about waiting until marriage to have sex-1st Mention"
   SEDABLC2 = "CF-20a Place where received instruction about waiting until marriage to have sex-2nd Mention"
   SEDABLC3 = "CF-20a Place where received instruction about waiting until marriage to have sex-3rd Mention"
   SEDABLC4 = "CF-20a Place where received instruction about waiting until marriage to have sex-4th Mention"
   SEDABSTG = "CF-21 R's Grade When Received Instruction on Waiting Until Marriage"
   SEDABSSX = "CF-22 R rec d instruction-Waiting Until Marriage before/after 1st sex"
   SAMEMAN = "CG-2 1st Partner Already Discussed as a Husband or Cohab Partner?"
   WHOFSTPR = "CG-3 Which man already discussed was R's 1st Sexual Partner?"
   KNOWFP = "CG-5 Relationship with 1st Partner at 1st Sex"
   STILFPSX = "CG-6 Is 1st Sexual Partner a Current Sexual Partner"
   LSTSEXFP_M = "CG-7m Month R Last Had Sex With 1st Sexual Partner"
   LSTSEXFP_Y = "CG-7y Year R Last Had Sex With 1st Sexual Partner"
   CMLSEXFP = "CM for Date of Last Sex With 1st Sexual Partner"
   CMFPLAST = "CM for Date of Last or Only Sex with 1st Sexual Partner"
   FPOTHREL = "CG-7a Relationship with Partner at Last Sex"
   FPEDUC = "CG-7b Highest level of education - 1st Sexual Partner"
   FSEXRACE = "RACE-recode-like variable for 1st sexual partner"
   FSEXNRACE = "HISPRACE2-recode-like variable for 1st sexual partner"
   FPRN = "CG-7f Describe Current Relationship with 1st Sexual Partner"
   WHICH1ST = "CG-8 First Sexual Intercourse Before or After Menarche"
   SEXAFMEN = "CG-9 Whether R Had Sex Since Menarche"
   PTSB4MAR = "CH-2 # of Male Sex Partners Prior to 1st Marriage"
   WHOSNC1Y = "CI-1 Is R's One Partner in Last 12 Mos Her Current H/P?"
   MATCHFP = "CI-4 Is last partner same as 1st partner ever?"
   MATCHHP = "CI-5 Which H/P already discussed, if any, is last partner?"
   P1YRELP = "Relationship of last partner to R"
   CMLSEX = "CM of last sex with last partner"
   P1YLSEX_M = "CI-6m Month of Last Sex-last partner"
   P1YLSEX_Y = "CI-6y Year of Last Sex-last partner"
   P1YCURRP = "CI-7 Whether partner is current-last partner"
   PCURRNT = "Whether partner is current - last partner"
   MATCHFP2 = "CI-4 Is 2nd-to-last partner same as 1st partner ever?"
   MATCHHP2 = "CI-5 Which H/P already discussed, if any, is 2nd-to-last partner?"
   P1YRELP2 = "Relationship of 2nd-to-last partner to R"
   CMLSEX2 = "CM of last sex with 2nd-to-last partner in last 12 mos"
   P1YLSEX_M2 = "CI-6m Month of Last Sex-2nd-to-last partner"
   P1YLSEX_Y2 = "CI-6y Year of Last Sex-2nd-to-last partner"
   P1YCURRP2 = "CI-7 Whether partner is current (2nd-to-last partner)"
   PCURRNT2 = "Whether partner is current (2nd-to-last partner)"
   MATCHFP3 = "CI-4 Is 3rd-to-last partner same as 1st partner ever?"
   MATCHHP3 = "CI-5 Which H/P already discussed, if any, is 3rd-to-last partner?"
   P1YRELP3 = "Relationship of 3rd-to-last partner to R"
   CMLSEX3 = "CM of last sex with 3rd-to-last partner in last 12 mos"
   P1YLSEX_M3 = "CI-6m Month of Last Sex-3rd-to-last partner"
   P1YLSEX_Y3 = "CI-6y Year of Last Sex-3rd-to-last partner"
   P1YCURRP3 = "CI-7 Whether partner is current (3rd-to-last partner)"
   PCURRNT3 = "Whether partner is current (3rd-to-last partner)"
   P1YOTHREL = "CI-8 Relationship at last Sex with last partner"
   P1YRAGE = "CI-9 R's Age at 1st Sex with last partner"
   P1YHSAGE = "CI-10 Partner's Age at 1st Sex with R -last partner"
   P1YRF = "CI-11 Relationship with last partner at 1st Sex with him"
   P1YFSEX_M = "CI-12m Month of 1st Sex with last partner"
   P1YFSEX_Y = "CI-12y Year of 1st Sex with last Partner"
   CMFSEX = "CM of first sex with last partner"
   CMFSEXTOT = "CM of first or only sex with last partner"
   P1YEDUC = "CI-13 Highest Level of Education-last Partner"
   P1YRACE1 = "RACE-recode-like variable for most recent partner in last 12 mos"
   P1YNRACE1 = "HISPRACE2-recode-like variable for most recent partner in last 12mos"
   P1YRN = "CI-17 Type of Current Relationship-last Partner"
   P1YOTHREL2 = "CI-8 Relationship at last Sex with 2nd-to-last partner"
   P1YRAGE2 = "CI-9 R's Age at 1st Sex with 2nd-to-last partner"
   P1YHSAGE2 = "CI-10 Partner's Age at 1st Sex with R -2nd-to-last partner"
   P1YRF2 = "CI-11 Relationship with 2nd-to-last partner at 1st Sex with him"
   P1YFSEX_M2 = "CI-12m Month of 1st Sex with 2nd-to-last partner"
   P1YFSEX_Y2 = "CI-12y Year of 1st Sex with 2nd-to-last partner"
   CMFSEX2 = "CM of first sex with 2nd-to-last partner"
   CMFSEXTOT2 = "CM of first or only sex with 2nd-to-last partner"
   P1YEDUC2 = "CI-13 Highest Level of Education-2nd-to-last partner"
   P1YRACE2 = "RACE-recode-like variable for 2nd-to-last partner in last 12 mos"
   P1YNRACE2 = "HISPRACE2-recode-like variable for 2nd-to-last partner in last 12 mos"
   P1YRN2 = "CI-17 Type of Current Relationship-2nd-to-last partner"
   P1YOTHREL3 = "CI-8 Relationship at last Sex with 3rd-to-last partner"
   P1YRAGE3 = "CI-9 R's Age at 1st Sex with 3rd-to-last partner"
   P1YHSAGE3 = "CI-10 Partner's Age at 1st Sex with R -3rd-to-last partner"
   P1YRF3 = "CI-11 Relationship with 3rd-to-last partner at 1st Sex with him"
   P1YFSEX_M3 = "CI-12m Month of 1st Sex with 3rd-to-last partner"
   P1YFSEX_Y3 = "CI-12y Year of 1st Sex with 3rd-to-last partner"
   CMFSEX3 = "CM of first sex with 3rd-to-last partner"
   CMFSEXTOT3 = "CM of first or only sex with 3rd-to-last partner"
   P1YEDUC3 = "CI-13 Highest Level of Education-3rd-to-last partner"
   P1YRACE3 = "RACE-recode-like variable for 3rd-to-last partner in last 12 mos"
   P1YNRACE3 = "HISPRACE2-recode-like variable for 3rd-to-last partner in last 12 mos"
   P1YRN3 = "CI-17 Type of Current Relationship-3rd-to-last partner"
   CURRPRTT = "# of Current Male Sexual Partners-including curr H/P"
   CURRPRTS = "# of Current Male Sexual Partners, not including curr H/P"
   CMCURRP1 = "CM for Date of Last or Only Sex With CURPART1"
   CMCURRP2 = "CM for Date of Last or Only Sex With CURPART2"
   CMCURRP3 = "CM for Date of Last or Only Sex With CURPART3"
   EVERTUBS = "DA-1 R Ever Had Tubes Tied/Cut/Removed"
   ESSURE = "DA-1b Ever had tubal sterilization procedure called ESSURE"
   EVERHYST = "DA-2 R Ever Had Hysterectomy"
   EVEROVRS = "DA-3 R Ever Had Both Ovaries Removed"
   EVEROTHR = "DA-4 R Ever Had Any Other Sterilizing Operation"
   WHTOOPRC = "DA-5a Codes for Spontaneous Description of Other Operation"
   ONOTFUNC = "DA-6 R Sterile from Operation on 1 Tube/Ovary"
   DFNLSTRL = "DA-7 R Sterile From This Other Operation"
   ANYTUBAL = "R ever had a tubal sterilization (Regardless of reversal)"
   HYST = "R is surgically sterile at interview due to hysterectomy"
   OVAREM = "R is surgically sterile at interview due to ovary removal"
   OTHR = "R is surgically sterile at interview due to other female operation"
   ANYFSTER = "R Ever Had Sterilizing Operation (Regardless of Reversal)"
   ANYOPSMN = "DA-8 R's Curr H/P Ever Had Vasectomy or Other Sterilizing Operation"
   WHATOPSM = "DA-9 Type of Sterilizing Operation R's Current H/P Had"
   DFNLSTRM = "DA-10 is R's Curr H/P completely sterile/impossible to become father"
   ANYMSTER = "R's H/P Ever Had Sterilizing Operation Regardless of Reversal"
   ANYVAS = "R's H/P ever had a vasectomy, regardless of later reversal"
   OTHRM = "R's H/P curr sterile from operation other than vasectomy (computed in Flow Check D-7)"
   DATFEMOP_Y = "DB-1 Year of R's tubal sterilization"
   PLCFEMOP = "DB-2 Site of R's tubal sterilization"
   PAYRSTER1 = "DB-2b How Paid for Tubal Sterilization-1st Mention"
   PAYRSTER2 = "DB-2b How Paid for Tubal Sterilization-2nd Mention"
   PAYRSTER3 = "DB-2b How Paid for Tubal Sterilization-3rd Mention"
   RHADALL = "DB-3a R Had All The Kids She Wanted-Tubal Steril"
   HHADALL = "DB-3b H/P Had All The Kids He Wanted-Tubal Steril"
   FMEDREAS1 = "DB-4 Medical reason for tubal sterilization-1st mention"
   FMEDREAS2 = "DB-4 Medical reason for tubal sterilization-2nd mention"
   FMEDREAS3 = "DB-4 Medical reason for tubal sterilization-3rd mention"
   FMEDREAS4 = "DB-4 Medical reason for tubal sterilization-4th mention"
   FMEDREAS5 = "DB-4 Medical reason for tubal sterilization-5th mention"
   BCREAS = "DB-5a Problems with Birth Control Method-tubal steril"
   BCWHYF = "DB-5b Medical Problems with Birth Control-tubal steril"
   MINCDNNR = "DB-6 Main reason for tubal sterilization"
   DATFEMOP_Y2 = "DB-1 Year of R's hysterectomy"
   PLCFEMOP2 = "DB-2 Site of R's hysterectomy"
   PAYRSTER6 = "DB-2b How Paid for Hysterectomy-1st Mention"
   PAYRSTER7 = "DB-2b How Paid for Hysterectomy-2nd Mention"
   PAYRSTER8 = "DB-2b How Paid for Hysterectomy-3rd Mention"
   RHADALL2 = "DB-3a R Had All The Kids She Wanted-Hysterectomy"
   HHADALL2 = "DB-3b H/P Had All The Kids He Wanted-Hysterectomy"
   FMEDREAS7 = "DB-4 Medical reason for hysterectomy-1st mention"
   FMEDREAS8 = "DB-4 Medical reason for hysterectomy-2nd mention"
   FMEDREAS9 = "DB-4 Medical reason for hysterectomy-3rd mention"
   FMEDREAS10 = "DB-4 Medical reason for hysterectomy-4th mention"
   FMEDREAS11 = "DB-4 Medical reason for hysterectomy-5th mention"
   BCREAS2 = "DB-5a Problems with Birth Control Method-hysterectomy"
   BCWHYF2 = "DB-5b Medical Problems with Birth Control-hysterectomy"
   MINCDNNR2 = "DB-6 Main reason for hysterectomy"
   DATFEMOP_Y3 = "DB-1 Year of R's ovary removal"
   PLCFEMOP3 = "DB-2 Site of R's ovary removal"
   PAYRSTER11 = "DB-2b How Paid for Ovary Removal-1st Mention"
   PAYRSTER12 = "DB-2b How Paid for Ovary Removal-2nd Mention"
   PAYRSTER13 = "DB-2b How Paid for Ovary Removal-3rd Mention"
   RHADALL3 = "DB-3a R Had All The Kids She Wanted-Ovary Removal"
   HHADALL3 = "DB-3b H/P Had All The Kids He Wanted-Ovary Removal"
   FMEDREAS13 = "DB-4 Medical reason for ovary removal-1st mention"
   FMEDREAS14 = "DB-4 Medical reason for ovary removal-2nd mention"
   FMEDREAS15 = "DB-4 Medical reason for ovary removal-3rd mention"
   FMEDREAS16 = "DB-4 Medical reason for ovary removal-4th mention"
   FMEDREAS17 = "DB-4 Medical reason for ovary removal-5th mention"
   BCREAS3 = "DB-5a Problems with Birth Control Method-ovary removal"
   BCWHYF3 = "DB-5b Medical Problems with Birth Control-ovary removal"
   MINCDNNR3 = "DB-6 Main reason for ovary removal"
   DATFEMOP_Y4 = "DB-1 Year of R's other sterilizing operation"
   PLCFEMOP4 = "DB-2 Site of R's other sterilizing operation"
   PAYRSTER16 = "DB-2b How Paid for Other Sterilizing Operation-1st Mention"
   PAYRSTER17 = "DB-2b How Paid for Other Sterilizing Operation-2nd Mention"
   PAYRSTER18 = "DB-2b How Paid for Other Sterilizing Operation-3rd Mention"
   RHADALL4 = "DB-3a R Had All The Kids She Wanted-Other Sterilizing Operation"
   HHADALL4 = "DB-3b H/P Had All The Kids He Wanted-Other Sterilizing Operation"
   FMEDREAS19 = "DB-4 Medical reason for Other Sterilizing operation-1st mention"
   FMEDREAS20 = "DB-4 Medical reason for Other Sterilizing operation-2nd mention"
   FMEDREAS21 = "DB-4 Medical reason for Other Sterilizing operation-3rd mention"
   FMEDREAS22 = "DB-4 Medical reason for Other Sterilizing operation-4th mention"
   FMEDREAS23 = "DB-4 Medical reason for Other Sterilizing operation-5th mention"
   BCREAS4 = "DB-5a Problems with Birth Control Method-Other Sterilizing Operation"
   BCWHYF4 = "DB-5b Medical Problems with Birth Control-Other Sterilizing Operation"
   MINCDNNR4 = "DB-6 Main reason for other sterilizing operation"
   OPERSAME1 = "DB-6b Tubal & Hysterectomy in Same Mo/Yr Done at Same Time"
   OPERSAME2 = "DB-6b Tubal & Ovary Removal in Same Mo/Yr Done at Same Time"
   OPERSAME3 = "DB-6b Tubal & Other Female Operation in Same Mo/Yr Done at Same Time"
   OPERSAME4 = "DB-6b Hysterectomy & Ovary Removal in Same Mo/Yr Done at Same Time"
   OPERSAME5 = "DB-6b Hysterectomy & Other Female Operation in Same Mo/Yr Done at Same Time"
   OPERSAME6 = "DB-6b Ovary Removal & Other Female Operation in Same Mo/Yr Done at Same Time"
   DATEOPMN_Y = "DB-7 Year When Current H/P Had Sterilizing Operation"
   WITHIMOP = "DB-8 Was R in Relationship with H/P When He Had Sterilization Operation"
   VASJAN4YR = "DB-8b Had Sterilization Operation since CMJAN4YR"
   PLACOPMN = "DB-9 Site of H/P's Sterilizing Operation"
   PAYMSTER1 = "DB-10 How paid for current H/P's operation-1st mention"
   PAYMSTER2 = "DB-10 How paid for current H/P's operation-2nd mention"
   RHADALLM = "DB-11a At H/P's Operation, R Had All The Children She Wanted"
   HHADALLM = "DB-11b At H/P's Operation, H/P Had All The Children He Wanted"
   MEDREAS1 = "DB-12 Medical reason for H/P's operation-1st mention"
   MEDREAS2 = "DB-12 Medical reason for H/P's operation-2nd mention"
   BCREASM = "DB-13a At H/P's Operation, Had Problems with Birth Control Method"
   BCWHYM = "DB-13b At H/P's Operation, Had Medical Problems with Birth Control"
   MINCDNMN = "DB-14 Main Reason for H/P's Sterilizing Operation"
   REVSTUBL = "DC-1 Ever had surgery to reverse tubal sterilization"
   DATRVSTB_Y = "DC-2 Year of Tubal Sterilization Reversal"
   REVSVASX = "DC-3 R's Current H/P Ever Had Surgery to Reverse Vasectomy"
   DATRVVEX_Y = "DC-4 Year of Current H/P's Vasectomy Reversal"
   TUBS = "R currently sterile from tubal ligation"
   VASECT = "R's H/P currently sterile from vasectomy"
   RSURGSTR = "Whether R is Surgically Sterile at Interview"
   PSURGSTR = "Whether R's Current H/P is Surgically Sterile at Interview"
   ONLYTBVS = "Whether R & H/P's Only Sterile Ops=Tubal or Vasectomy"
   RWANTRVT = "DC-5 R Wants to Reverse Her Tubal Ligation"
   MANWANTT = "DC-6 R's H/P Wants R to Reverse Her Tubal Ligation"
   RWANTREV = "DC-7 R Wants H/P to Reverse His Vasectomy"
   MANWANTR = "DC-8 R's H/P Wants to Reverse His Vasectomy"
   POSIBLPG = "DE-1 Physically Possible for R to Have a Baby"
   REASIMPR = "DE-2 Why Impossible for R to Have a Baby"
   POSIBLMN = "DE-3 Physically Possible for R's Current H/P to Father a Baby"
   REASIMPP = "DE-4 Why Impossible for R's Current H/P to Father a Baby"
   CANHAVER = "DF-1 Physically Difficult for R to Have a Baby"
   REASDIFF1 = "DF-2 Why Physically Difficult for R to Have Baby-1st mention"
   REASDIFF2 = "DF-2 Why Physically Difficult for R to Have Baby-2nd mention"
   REASDIFF3 = "DF-2 Why Physically Difficult for R to Have Baby-3rd mention"
   REASDIFF4 = "DF-2 Why Physically Difficult for R to Have Baby-4th mention"
   REASDIFF5 = "DF-2 Why Physically Difficult for R to Have Baby-5th mention"
   CANHAVEM = "DF-3 R's Current H/P Has Physical Difficulty Fathering a Baby"
   PREGNONO = "DF-4 Doctor Ever Advised R Never to Become Pregnant"
   REASNONO1 = "DF-5 Why R was Advised Against Pregnancy-1st mention"
   REASNONO2 = "DF-5 Why R was Advised Against Pregnancy-2nd mention"
   REASNONO3 = "DF-5 Why R was Advised Against Pregnancy-3rd mention"
   RSTRSTAT = "R's Sterility Status at Time of Interview"
   PSTRSTAT = "R's Current H/P's Sterility Status at Time of Interview"
   LASTPER = "DF-6 Recency of last menstrual period"
   TRYPREG12 = "DF-7 Tried to get pregnant within past 12 months"
   PILL = "EA-1 R ever used Birth Control Pills?"
   CONDOM = "EA-2 R ever used Condoms?"
   VASECTMY = "EA-3 R ever used partner's vasectomy?"
   DEPOPROV = "EA-4 R ever used Depo-Provera, Injectables?"
   WIDRAWAL = "EA-6 R ever used withdrawal?"
   RHYTHM = "EA-7 R ever used calendar rhythm method?"
   SDAYCBDS = "EA-7b R ever used Standard Days or CycleBeads method?"
   TEMPSAFE = "EA-8 R ever used symptothermal method?"
   PATCH = "EA-9 R ever used contraceptive patch?"
   RING = "EA-10 R ever used vaginal contraceptive ring or "NuvaRing"?"
   MORNPILL = "EA-11 R ever used emergency contraception?"
   ECTIMESX = "EA-12 Number of times R used emergency contraception"
   ECREASON1 = "EA-13 Reason R Used Emergency Contraception-1st Reason"
   ECREASON2 = "EA-13 Reason R Used Emergency Contraception-2nd Reason"
   ECREASON3 = "EA-13 Reason R Used Emergency Contraception-3rd Reason"
   ECRX = "EA-13aa Get emergency contraception with or without prescription?"
   ECWHERE = "EA-13a Where R last got emergency contraception"
   ECWHEN = "EA-13b R last got emergency contraception within past 12 mons or longer ago"
   OTHRMETH01 = "EA-14 Which other method ever used(if any)? - 1st mention"
   OTHRMETH02 = "EA-14 Which other method ever used(if any)? - 2nd mention"
   OTHRMETH03 = "EA-14 Which other method ever used(if any)? - 3rd mention"
   OTHRMETH04 = "EA-14 Which other method ever used(if any)? - 4th mention"
   OTHRMETH05 = "EA-14 Which other method ever used(if any)? - 5th mention"
   OTHRMETH06 = "EA-14 Which other method ever used(if any)? - 6th mention"
   OTHRMETH07 = "EA-14 Which other method ever used(if any)? - 7th mention"
   EVIUDTYP1 = "EA-15a Types of IUD ever used - 1st mention"
   EVIUDTYP2 = "EA-15a Types of IUD ever used - 2nd mention"
   EVIUDTYP3 = "EA-15a Types of IUD ever used - 3rd mention"
   NEWMETH = "Code for other method specified in EA-15 SP_OTHRMETH"
   EVERUSED = "R ever used any method (from EA  series & sterilization info from D)"
   METHDISS = "EA-16 R ever stop using method because dissatisfied with it"
   METHSTOP01 = "EA-17 Method Stopped Using Due to Dissatisfaction-1st Method"
   METHSTOP02 = "EA-17 Method Stopped Using Due to Dissatisfaction-2nd Method"
   METHSTOP03 = "EA-17 Method Stopped Using Due to Dissatisfaction-3rd Method"
   METHSTOP04 = "EA-17 Method Stopped Using Due to Dissatisfaction-4th Method"
   METHSTOP05 = "EA-17 Method Stopped Using Due to Dissatisfaction-5th Method"
   METHSTOP06 = "EA-17 Method Stopped Using Due to Dissatisfaction-6th Method"
   METHSTOP07 = "EA-17 Method Stopped Using Due to Dissatisfaction-7th Method"
   METHSTOP08 = "EA-17 Method Stopped Using Due to Dissatisfaction-8th Method"
   METHSTOP09 = "EA-17 Method Stopped Using Due to Dissatisfaction-9th Method"
   METHSTOP10 = "EA-17 Method Stopped Using Due to Dissatisfaction-10th Method"
   WHENPILL = "EA-17a If discontinued pill within past 12 months"
   REASPILL01 = "EA-18 Reason not satisfied with the pill - 1st mention"
   REASPILL02 = "EA-18 Reason not satisfied with the pill - 2nd mention"
   REASPILL03 = "EA-18 Reason not satisfied with the pill - 3rd mention"
   REASPILL04 = "EA-18 Reason not satisfied with the pill - 4th mention"
   REASPILL05 = "EA-18 Reason not satisfied with the pill - 5th mention"
   REASPILL06 = "EA-18 Reason not satisfied with the pill - 6th mention"
   REASPILL07 = "EA-18 Reason not satisfied with the pill - 7th mention"
   REASPILL08 = "EA-18 Reason not satisfied with the pill - 8th mention"
   REASPILL09 = "EA-18 Reason not satisfied with the pill - 9th mention"
   STOPPILL1 = "Open-ended response to reason(s) for discontinuation of pill: 1st mention"
   STOPPILL2 = "Open-ended response to reason(s) for discontinuation of pill: 2nd mention"
   STOPPILL3 = "Open-ended response to reason(s) for discontinuation of pill: 3rd mention"
   STOPPILL4 = "Open-ended response to reason(s) for discontinuation of pill: 4th mention"
   STOPPILL5 = "Open-ended response to reason(s) for discontinuation of pill: 5th mention"
   STOPPILL6 = "Open-ended response to reason(s) for discontinuation of pill: 6th mention"
   WHENCOND = "EA-18e If discontinued condom within past 12 months"
   REASCOND01 = "EA-19 Reason not satisfied with condom - 1st mention"
   REASCOND02 = "EA-19 Reason not satisfied with condom - 2nd mention"
   REASCOND03 = "EA-19 Reason not satisfied with condom - 3rd mention"
   REASCOND04 = "EA-19 Reason not satisfied with condom - 4th mention"
   REASCOND05 = "EA-19 Reason not satisfied with condom - 5th mention"
   REASCOND06 = "EA-19 Reason not satisfied with condom - 6th mention"
   REASCOND07 = "EA-19 Reason not satisfied with condom - 7th mention"
   STOPCOND1 = "Open-ended response to reason(s) for discontinuation of condom: 1st mention"
   STOPCOND2 = "Open-ended response to reason(s) for discontinuation of condom: 2nd mention"
   WHENIUD = "EA-20e If discontinued IUD within past 12 months"
   TYPEIUD_1 = "EA-21 Type of IUD that was discontinued due to dissatisfaction - 1st mention"
   TYPEIUD_2 = "EA-21 Type of IUD that was discontinued due to dissatisfaction - 2nd mention"
   TYPEIUD_3 = "EA-21 Type of IUD that was discontinued due to dissatisfaction - 3rd mention"
   REASIUD01 = "EA-21a Reason not satisfied with IUD - 1st mention"
   REASIUD02 = "EA-21a Reason not satisfied with IUD - 2nd mention"
   REASIUD03 = "EA-21a Reason not satisfied with IUD - 3rd mention"
   REASIUD04 = "EA-21a Reason not satisfied with IUD - 4th mention"
   REASIUD05 = "EA-21a Reason not satisfied with IUD - 5th mention"
   REASIUD06 = "EA-21a Reason not satisfied with IUD - 6th mention"
   REASIUD07 = "EA-21a Reason not satisfied with IUD - 7th mention"
   REASIUD08 = "EA-21a Reason not satisfied with IUD - 8th mention"
   STOPIUD1 = "Open-ended response to reason(s) for discontinuation of IUD: 1st mention"
   STOPIUD2 = "Open-ended response to reason(s) for discontinuation of IUD: 2nd mention"
   STOPIUD3 = "Open-ended response to reason(s) for discontinuation of IUD: 3rd mention"
   STOPIUD4 = "Open-ended response to reason(s) for discontinuation of IUD: 4th mention"
   STOPIUD5 = "Open-ended response to reason(s) for discontinuation of IUD: 5th mention"
   LARC10 = "EA-22a Whether used IUD/implant in past 10 years"
   LARCREMV = "EA-22b Whether wanted to get IUD/implant removed in past 10 years"
   REMOVWHY_1 = "EA-22c Reasons wanted to get IUD/implant removed in past 10 years: 1st mention"
   REMOVWHY_2 = "EA-22c Reasons wanted to get IUD/implant removed in past 10 years: 2nd mention"
   REMOVWHY_3 = "EA-22c Reasons wanted to get IUD/implant removed in past 10 years: 3rd mention"
   REMOVWHY_4 = "EA-22c Reasons wanted to get IUD/implant removed in past 10 years: 4th mention"
   REMOVWHY_5 = "EA-22c Reasons wanted to get IUD/implant removed in past 10 years: 5th mention"
   REMOVDIF = "EA-22d Ever difficulty getting IUD/implant removed in past 10 years"
   REMVDIFY_1 = "EA-22e Reasons you had difficulty getting an implant or IUD removed: 1st mention"
   REMVDIFY_2 = "EA-22e Reasons you had difficulty getting an implant or IUD removed: 2nd mention"
   REMVDIFY_3 = "EA-22e Reasons you had difficulty getting an implant or IUD removed: 3rd mention"
   FIRSMETH1 = "EB-1 First method ever used - 1st mention"
   FIRSMETH2 = "EB-1 First method ever used - 2nd mention"
   FIRSMETH3 = "EB-1 First method ever used - 3rd mention"
   FIRSMETH4 = "EB-1 First method ever used - 4th mention"
   NUMFIRSM = "Total number of responses in EB-1 FIRSMETH"
   DRUGDEV = "Yes if 1st method was drug or device, no otherwise (1st meth)"
   DRUGDEV2 = "Yes if 1st method was drug or device, no otherwise (2nd meth)"
   DRUGDEV3 = "Yes if 1st method was drug or device, no otherwise (3rd meth)"
   DRUGDEV4 = "Yes if 1st method was drug or device, no otherwise (4th meth)"
   FIRSTIME1 = "EB-2 when 1st method use vis-a-vis 1st sex (noncontinuous meth)"
   FIRSTIME2 = "EB-2 when first method use vis-a-vis 1st sex (continuous method)"
   USEFSTSX = "Whether R used a method at first sex (total universe)"
   CMFIRSM = "CM for date of first method use (total universe)"
   MTHFSTSX1 = "Method used at first sex-1st mention (tot universe)"
   MTHFSTSX2 = "Method used at first sex-2nd mention (tot universe)"
   MTHFSTSX3 = "Method used at first sex-3rd mention (tot universe)"
   MTHFSTSX4 = "Method used at first sex-4th mention (tot universe)"
   WNFSTUSE_M = "EB-3 Month R First Used a Method"
   WNFSTUSE_Y = "EB-3 Year R First Used a Method"
   CMFSTUSE = "Century month for date first used a method (if not at first sex) (partial univ)"
   FMETHPRS = "Whether 1st method prescrip or non prescrip (1st meth)"
   FMETHPRS2 = "Whether 1st method prescrip or non prescrip (2nd meth)"
   FMETHPRS3 = "Whether 1st method prescrip or non prescrip (3rd meth)"
   FMETHPRS4 = "Whether 1st method prescrip or non prescrip (4th meth)"
   AGEFSTUS = "EB-4 Age When First Used Method"
   PLACGOTF = "EB-5 Where R Got 1st Method-1st method mentioned"
   PLACGOTF2 = "EB-5 Where R Got 1st Method-2nd method mentioned"
   PLACGOTF3 = "EB-5 Where R Got 1st Method-3rd method mentioned"
   PLACGOTF4 = "EB-5 Where R Got 1st Method-4th method mentioned"
   USEFRSTS = "EB-6 R Use Any Method at First Sex (1st meth use was bef 1st sx)"
   MTHFRSTS1 = "EB-8 Method Used at  First Sex (1st meth use was bef 1st sx) -1st mention"
   MTHFRSTS2 = "EB-8 Method Used at  First Sex (1st meth use was bef 1st sx) -2nd mention"
   MTHFRSTS3 = "EB-8 Method Used at  First Sex (1st meth use was bef 1st sx) -3rd mention"
   MTHFRSTS4 = "EB-8 Method Used at  First Sex (1st meth use was bef 1st sx) -4th mention"
   INTR_EC3 = "EC-3 Any times since [Jan,intvw yr-3/date 1st sex] no sex for 1  mon?"
   CMMONSX = "Century month of this entry in the months of nonintercourse series (Jan, intvw yr-3)"
   MONSX = "EC-8 Sex in January [intvw yr-3]?"
   CMMONSX2 = "Century month of this entry in the months of nonintercourse series (Feb, intvw yr-3)"
   MONSX2 = "EC-8 Sex in February [intvw yr-3]?"
   CMMONSX3 = "Century month of this entry in the months of nonintercourse series (March, intvw yr-3)"
   MONSX3 = "EC-8 Sex in March [intvw yr-3]?"
   CMMONSX4 = "Century month of this entry in the months of nonintercourse series (April, intvw yr-3)"
   MONSX4 = "EC-8 Sex in April [intvw yr-3]?"
   CMMONSX5 = "Century month of this entry in the months of nonintercourse series (May, intvw yr-3)"
   MONSX5 = "EC-8 Sex in May [intvw yr-3]?"
   CMMONSX6 = "Century month of this entry in the months of nonintercourse series (June, intvw yr-3)"
   MONSX6 = "EC-8 Sex in June [intvw yr-3]?"
   CMMONSX7 = "Century month of this entry in the months of nonintercourse series (July, intvw yr-3)"
   MONSX7 = "EC-8 Sex in July [intvw yr-3]?"
   CMMONSX8 = "Century month of this entry in the months of nonintercourse series (Aug, intvw yr-3)"
   MONSX8 = "EC-8 Sex in August [intvw yr-3]?"
   CMMONSX9 = "Century month of this entry in the months of nonintercourse series (Sept, intvw yr-3)"
   MONSX9 = "EC-8 Sex in September [intvw yr-3]?"
   CMMONSX10 = "Century month of this entry in the months of nonintercourse series (Oct, intvw yr-3)"
   MONSX10 = "EC-8 Sex in October [intvw yr-3]?"
   CMMONSX11 = "Century month of this entry in the months of nonintercourse series (Nov, intvw yr-3)"
   MONSX11 = "EC-8 Sex in November [intvw yr-3]?"
   CMMONSX12 = "Century month of this entry in the months of nonintercourse series (Dec, intvw yr-3)"
   MONSX12 = "EC-8 Sex in December [intvw yr-3]?"
   CMMONSX13 = "Century month of this entry in the months of nonintercourse series (Jan, intvw yr-2)"
   MONSX13 = "EC-8 Sex in January [intvw yr-2]?"
   CMMONSX14 = "Century month of this entry in the months of nonintercourse series (Feb, intvw yr-2)"
   MONSX14 = "EC-8 Sex in February [intvw yr-2]?"
   CMMONSX15 = "Century month of this entry in the months of nonintercourse series (March, intvw yr-2)"
   MONSX15 = "EC-8 Sex in March [intvw yr-2]?"
   CMMONSX16 = "Century month of this entry in the months of nonintercourse series (April, intvw yr-2)"
   MONSX16 = "EC-8 Sex in April [intvw yr-2]?"
   CMMONSX17 = "Century month of this entry in the months of nonintercourse series (May, intvw yr-2)"
   MONSX17 = "EC-8 Sex in May [intvw yr-2]?"
   CMMONSX18 = "Century month of this entry in the months of nonintercourse series (June, intvw yr-2)"
   MONSX18 = "EC-8 Sex in June [intvw yr-2]?"
   CMMONSX19 = "Century month of this entry in the months of nonintercourse series (July, intvw yr-2)"
   MONSX19 = "EC-8 Sex in July [intvw yr-2]?"
   CMMONSX20 = "Century month of this entry in the months of nonintercourse series (Aug, intvw yr-2)"
   MONSX20 = "EC-8 Sex in August [intvw yr-2]?"
   CMMONSX21 = "Century month of this entry in the months of nonintercourse series (Sept, intvw yr-2)"
   MONSX21 = "EC-8 Sex in September [intvw yr-2]?"
   CMMONSX22 = "Century month of this entry in the months of nonintercourse series (Oct, intvw yr-2)"
   MONSX22 = "EC-8 Sex in October [intvw yr-2]?"
   CMMONSX23 = "Century month of this entry in the months of nonintercourse series (Nov, intvw yr-2)"
   MONSX23 = "EC-8 Sex in November [intvw yr-2]?"
   CMMONSX24 = "Century month of this entry in the months of nonintercourse series (Dec, intvw yr-2)"
   MONSX24 = "EC-8 Sex in December [intvw yr-2]?"
   CMMONSX25 = "Century month of this entry in the months of nonintercourse series (Jan, intvw yr-1)"
   MONSX25 = "EC-8 Sex in January [intvw yr-1]?"
   CMMONSX26 = "Century month of this entry in the months of nonintercourse series (Feb, intvw yr-1)"
   MONSX26 = "EC-8 Sex in February [intvw yr-1]?"
   CMMONSX27 = "Century month of this entry in the months of nonintercourse series (March, intvw yr-1)"
   MONSX27 = "EC-8 Sex in March [intvw yr-1]?"
   CMMONSX28 = "Century month of this entry in the months of nonintercourse series (April, intvw yr-1)"
   MONSX28 = "EC-8 Sex in April [intvw yr-1]?"
   CMMONSX29 = "Century month of this entry in the months of nonintercourse series (May, intvw yr-1)"
   MONSX29 = "EC-8 Sex in May [intvw yr-1]?"
   CMMONSX30 = "Century month of this entry in the months of nonintercourse series (June, intvw yr-1)"
   MONSX30 = "EC-8 Sex in June [intvw yr-1]?"
   CMMONSX31 = "Century month of this entry in the months of nonintercourse series (July, intvw yr-1)"
   MONSX31 = "EC-8 Sex in July [intvw yr-1]?"
   CMMONSX32 = "Century month of this entry in the months of nonintercourse series (Aug, intvw yr-1)"
   MONSX32 = "EC-8 Sex in August [intvw yr-1]?"
   CMMONSX33 = "Century month of this entry in the months of nonintercourse series (Sept, intvw yr-1)"
   MONSX33 = "EC-8 Sex in September [intvw yr-1]?"
   CMMONSX34 = "Century month of this entry in the months of nonintercourse series (Oct, intvw yr-1)"
   MONSX34 = "EC-8 Sex in October [intvw yr-1]?"
   CMMONSX35 = "Century month of this entry in the months of nonintercourse series (Nov, intvw yr-1)"
   MONSX35 = "EC-8 Sex in November [intvw yr-1]?"
   CMMONSX36 = "Century month of this entry in the months of nonintercourse series (Dec, intvw yr-1)"
   MONSX36 = "EC-8 Sex in December [intvw yr-1]?"
   CMMONSX37 = "Century month of this entry in the months of nonintercourse series (Jan, intvw yr)"
   MONSX37 = "EC-8 Sex in January [intvw yr]?"
   CMMONSX38 = "Century month of this entry in the months of nonintercourse series (Feb, intvw yr)"
   MONSX38 = "EC-8 Sex in February [intvw yr]?"
   CMMONSX39 = "Century month of this entry in the months of nonintercourse series (March, intvw yr)"
   MONSX39 = "EC-8 Sex in March [intvw yr]?"
   CMMONSX40 = "Century month of this entry in the months of nonintercourse series (April, intvw yr)"
   MONSX40 = "EC-8 Sex in April [intvw yr]?"
   CMMONSX41 = "Century month of this entry in the months of nonintercourse series (May, intvw yr)"
   MONSX41 = "EC-8 Sex in May [intvw yr]?"
   CMMONSX42 = "Century month of this entry in the months of nonintercourse series (June, intvw yr)"
   MONSX42 = "EC-8 Sex in June [intvw yr]?"
   CMMONSX43 = "Century month of this entry in the months of nonintercourse series (July, intvw yr)"
   MONSX43 = "EC-8 Sex in July [intvw yr]?"
   CMMONSX44 = "Century month of this entry in the months of nonintercourse series (Aug, intvw yr)"
   MONSX44 = "EC-8 Sex in August [intvw yr]?"
   CMMONSX45 = "Century month of this entry in the months of nonintercourse series (Sept, intvw yr)"
   MONSX45 = "EC-8 Sex in September [intvw yr]?"
   CMMONSX46 = "Century month of this entry in the months of nonintercourse series (Oct, intvw yr)"
   MONSX46 = "EC-8 Sex in October [intvw yr]?"
   CMMONSX47 = "Century month of this entry in the months of nonintercourse series (Nov, intvw yr)"
   MONSX47 = "EC-8 Sex in November [intvw yr]?"
   CMMONSX48 = "Century month of this entry in the months of nonintercourse series (Dec, intvw yr)"
   MONSX48 = "EC-8 Sex in December [intvw yr]?"
   CMSTRTMC = "Century month for date of starting month of method calendar"
   CMENDMC = "Century month for date of ending month of method calendar"
   INTR_ED4A = "ED-4a For Rs who had hysterectomies before start of meth cal: use any method since Jan [interview yr - 3]/start of method calendar]?"
   NUMMCMON = "Number of months of meth. calendar asked about"
   MC1MONS1 = "ED-9a # of months using method specified in 1st MC month (one method mentioned)"
   MC1SIMSQ = "ED-9b Using methods in 1st MC month at same or diff times (2+ methods mentioned)"
   MC1MONS2 = "ED-9c # of months using methods together (1st MC month) (2+ methods mentioned, used at same time)"
   MC1MONS3 = "ED-9d # of months using method combinations (1st MC month) (2+ methods mentioned, used at diff times)"
   DATBEGIN_M = "ED-9 Month R Began Using method/method combo used in Jan [interview yr-3]"
   DATBEGIN_Y = "ED-9 Year R Began Using method/method combo used in Jan [interview yr-3]"
   CMDATBEGIN = "CM date  R Began Using method/method combo used in Jan [interview yr-3]"
   CURRMETH1 = "Method used in month of interview (1st mention)"
   CURRMETH2 = "Method used in month of interview (2nd mention)"
   CURRMETH3 = "Method used in month of interview (3rd mention)"
   CURRMETH4 = "Method used in month of interview (4th mention)"
   LSTMONMETH1 = "Method used in month before interview (1st mention)"
   LSTMONMETH2 = "Method used in month before interview (2nd mention)"
   LSTMONMETH3 = "Method used in month before interview (3rd mention)"
   LSTMONMETH4 = "Method used in month before interview (4th mention)"
   PILL_12 = "FC E-55 R used pill in 12 mos before interview?"
   DIAPH_12 = "FC E-55 R used diaphragm in the 12 mos before interview?"
   IUD_12 = "FC E-55 R used IUD in the 12 mos before interview?"
   IMPLANT_12 = "FC E-55 R used hormonal implants (Norplant or Implanon) in 12 mos before interview?"
   DEPO_12 = "FC E-55 R used Depo-provera injectables in the 12 mos before interview?"
   CERVC_12 = "FC E-55 R used cervical cap in the 12 mos before interview"
   MPILL_12 = "FC E-55 R used emergency contraceptive in the 12 mos before interview?"
   PATCH_12 = "FC E-55 R used contraceptive patch in the 12 mos before interview?"
   RING_12 = "FC E-55 R used contraceptive ring in the 12 mos before interview?"
   METHX1 = "ED-6 Method(s) used in Jan [interview yr-3] - 1st mention"
   METHX2 = "ED-6 Method(s) used in Jan [interview yr-3]-2nd mention"
   METHX3 = "ED-6 Method(s) used in Jan [interview yr-3]-3rd mention"
   METHX4 = "ED-6 Method(s) used in Jan [interview yr-3]-4th mention"
   METHX5 = "ED-6 Method(s) used in Feb [interview yr-3] - 1st mention"
   METHX6 = "ED-6 Method(s) used in Feb [interview yr-3]-2nd mention"
   METHX7 = "ED-6 Method(s) used in Feb [interview yr-3] - 3rd mention"
   METHX8 = "ED-6 Method(s) used in Feb [interview yr-3]-4th mention"
   METHX9 = "ED-6 Method(s) used in March [interview yr-3] - 1st mention"
   METHX10 = "ED-6 Method(s) used in March [interview yr-3]-2nd mention"
   METHX11 = "ED-6 Method(s) used in March [interview yr-3] - 3rd mention"
   METHX12 = "ED-6 Method(s) used in March [interview yr-3]-4th mention"
   METHX13 = "ED-6 Method(s) used in April [interview yr-3] - 1st mention"
   METHX14 = "ED-6 Method(s) used in April [interview yr-3]-2nd mention"
   METHX15 = "ED-6 Method(s) used in April [interview yr-3] - 3rd mention"
   METHX16 = "ED-6 Method(s) used in April [interview yr-3]-4th mention"
   METHX17 = "ED-6 Method(s) used in May [interview yr-3] - 1st mention"
   METHX18 = "ED-6 Method(s) used in May [interview yr-3]-2nd mention"
   METHX19 = "ED-6 Method(s) used in May [interview yr-3] - 3rd mention"
   METHX20 = "ED-6 Method(s) used in May [interview yr-3]-4th mention"
   METHX21 = "ED-6 Method(s) used in June [interview yr-3] - 1st mention"
   METHX22 = "ED-6 Method(s) used in June [interview yr-3]-2nd mention"
   METHX23 = "ED-6 Method(s) used in June [interview yr-3] - 3rd mention"
   METHX24 = "ED-6 Method(s) used in June [interview yr-3]-4th mention"
   METHX25 = "ED-6 Method(s) used in July [interview yr-3] - 1st mention"
   METHX26 = "ED-6 Method(s) used in July [interview yr-3]-2nd mention"
   METHX27 = "ED-6 Method(s) used in July [interview yr-3] - 3rd mention"
   METHX28 = "ED-6 Method(s) used in July [interview yr-3]-4th mention"
   METHX29 = "ED-6 Method(s) used in Aug [interview yr-3] - 1st mention"
   METHX30 = "ED-6 Method(s) used in Aug [interview yr-3]-2nd mention"
   METHX31 = "ED-6 Method(s) used in Aug [interview yr-3] - 3rd mention"
   METHX32 = "ED-6 Method(s) used in Aug [interview yr-3]-4th mention"
   METHX33 = "ED-6 Method(s) used in Sept [interview yr-3] - 1st mention"
   METHX34 = "ED-6 Method(s) used in Sept [interview yr-3]-2nd mention"
   METHX35 = "ED-6 Method(s) used in Sept [interview yr-3] - 3rd mention"
   METHX36 = "ED-6 Method(s) used in Sept [interview yr-3]-4th mention"
   METHX37 = "ED-6 Method(s) used in Oct [interview yr-3] - 1st mention"
   METHX38 = "ED-6 Method(s) used in Oct [interview yr-3]-2nd mention"
   METHX39 = "ED-6 Method(s) used in Oct [interview yr-3] - 3rd mention"
   METHX40 = "ED-6 Method(s) used in Oct [interview yr-3]-4th mention"
   METHX41 = "ED-6 Method(s) used in Nov [interview yr-3] - 1st mention"
   METHX42 = "ED-6 Method(s) used in Nov [interview yr-3]-2nd mention"
   METHX43 = "ED-6 Method(s) used in Nov [interview yr-3] - 3rd mention"
   METHX44 = "ED-6 Method(s) used in Nov [interview yr-3]-4th mention"
   METHX45 = "ED-6 Method(s) used in Dec [interview yr-3] - 1st mention"
   METHX46 = "ED-6 Method(s) used in Dec [interview yr-3]-2nd mention"
   METHX47 = "ED-6 Method(s) used in Dec [interview yr-3] - 3rd mention"
   METHX48 = "ED-6 Method(s) used in Dec [interview yr-3]-4th mention"
   METHX49 = "ED-6 Method(s) used in Jan [interview yr-2] - 1st mention"
   METHX50 = "ED-6 Method(s) used in Jan [interview yr-2]-2nd mention"
   METHX51 = "ED-6 Method(s) used in Jan [interview yr-2]-3rd mention"
   METHX52 = "ED-6 Method(s) used in Jan [interview yr-2]-4th mention"
   METHX53 = "ED-6 Method(s) used in Feb [interview yr-2] - 1st mention"
   METHX54 = "ED-6 Method(s) used in Feb [interview yr-2]-2nd mention"
   METHX55 = "ED-6 Method(s) used in Feb [interview yr-2] - 3rd mention"
   METHX56 = "ED-6 Method(s) used in Feb [interview yr-2]-4th mention"
   METHX57 = "ED-6 Method(s) used in March [interview yr-2] - 1st mention"
   METHX58 = "ED-6 Method(s) used in March [interview yr-2]-2nd mention"
   METHX59 = "ED-6 Method(s) used in March [interview yr-2] - 3rd mention"
   METHX60 = "ED-6 Method(s) used in March [interview yr-2]-4th mention"
   METHX61 = "ED-6 Method(s) used in April [interview yr-2] - 1st mention"
   METHX62 = "ED-6 Method(s) used in April [interview yr-2]-2nd mention"
   METHX63 = "ED-6 Method(s) used in April [interview yr-2] - 3rd mention"
   METHX64 = "ED-6 Method(s) used in April [interview yr-2]-4th mention"
   METHX65 = "ED-6 Method(s) used in May [interview yr-2] - 1st mention"
   METHX66 = "ED-6 Method(s) used in May [interview yr-2]-2nd mention"
   METHX67 = "ED-6 Method(s) used in May [interview yr-2] - 3rd mention"
   METHX68 = "ED-6 Method(s) used in May [interview yr-2]-4th mention"
   METHX69 = "ED-6 Method(s) used in June [interview yr-2] - 1st mention"
   METHX70 = "ED-6 Method(s) used in June [interview yr-2]-2nd mention"
   METHX71 = "ED-6 Method(s) used in June [interview yr-2] - 3rd mention"
   METHX72 = "ED-6 Method(s) used in June [interview yr-2]-4th mention"
   METHX73 = "ED-6 Method(s) used in July [interview yr-2] - 1st mention"
   METHX74 = "ED-6 Method(s) used in July [interview yr-2]-2nd mention"
   METHX75 = "ED-6 Method(s) used in July [interview yr-2] - 3rd mention"
   METHX76 = "ED-6 Method(s) used in July [interview yr-2]-4th mention"
   METHX77 = "ED-6 Method(s) used in Aug [interview yr-2] - 1st mention"
   METHX78 = "ED-6 Method(s) used in Aug [interview yr-2]-2nd mention"
   METHX79 = "ED-6 Method(s) used in Aug [interview yr-2] - 3rd mention"
   METHX80 = "ED-6 Method(s) used in Aug [interview yr-2]-4th mention"
   METHX81 = "ED-6 Method(s) used in Sept [interview yr-2] - 1st mention"
   METHX82 = "ED-6 Method(s) used in Sept [interview yr-2]-2nd mention"
   METHX83 = "ED-6 Method(s) used in Sept [interview yr-2] - 3rd mention"
   METHX84 = "ED-6 Method(s) used in Sept [interview yr-2]-4th mention"
   METHX85 = "ED-6 Method(s) used in Oct [interview yr-2] - 1st mention"
   METHX86 = "ED-6 Method(s) used in Oct [interview yr-2]-2nd mention"
   METHX87 = "ED-6 Method(s) used in Oct [interview yr-2] - 3rd mention"
   METHX88 = "ED-6 Method(s) used in Oct [interview yr-2]-4th mention"
   METHX89 = "ED-6 Method(s) used in Nov [interview yr-2] - 1st mention"
   METHX90 = "ED-6 Method(s) used in Nov [interview yr-2]-2nd mention"
   METHX91 = "ED-6 Method(s) used in Nov [interview yr-2] - 3rd mention"
   METHX92 = "ED-6 Method(s) used in Nov [interview yr-2]-4th mention"
   METHX93 = "ED-6 Method(s) used in Dec [interview yr-2] - 1st mention"
   METHX94 = "ED-6 Method(s) used in Dec [interview yr-2]-2nd mention"
   METHX95 = "ED-6 Method(s) used in Dec [interview yr-2] - 3rd mention"
   METHX96 = "ED-6 Method(s) used in Dec [interview yr-2]-4th mention"
   METHX97 = "ED-6 Method(s) used in Jan [interview yr-1] - 1st mention"
   METHX98 = "ED-6 Method(s) used in Jan [interview yr-1]-2nd mention"
   METHX99 = "ED-6 Method(s) used in Jan [interview yr-1]-3rd mention"
   METHX100 = "ED-6 Method(s) used in Jan [interview yr-1]-4th mention"
   METHX101 = "ED-6 Method(s) used in Feb [interview yr-1] - 1st mention"
   METHX102 = "ED-6 Method(s) used in Feb [interview yr-1]-2nd mention"
   METHX103 = "ED-6 Method(s) used in Feb [interview yr-1] - 3rd mention"
   METHX104 = "ED-6 Method(s) used in Feb [interview yr-1]-4th mention"
   METHX105 = "ED-6 Method(s) used in March [interview yr-1] - 1st mention"
   METHX106 = "ED-6 Method(s) used in March [interview yr-1]-2nd mention"
   METHX107 = "ED-6 Method(s) used in March [interview yr-1] - 3rd mention"
   METHX108 = "ED-6 Method(s) used in March [interview yr-1]-4th mention"
   METHX109 = "ED-6 Method(s) used in April [interview yr-1] - 1st mention"
   METHX110 = "ED-6 Method(s) used in April [interview yr-1]-2nd mention"
   METHX111 = "ED-6 Method(s) used in April [interview yr-1] - 3rd mention"
   METHX112 = "ED-6 Method(s) used in April [interview yr-1]-4th mention"
   METHX113 = "ED-6 Method(s) used in May [interview yr-1] - 1st mention"
   METHX114 = "ED-6 Method(s) used in May [interview yr-1]-2nd mention"
   METHX115 = "ED-6 Method(s) used in May [interview yr-1] - 3rd mention"
   METHX116 = "ED-6 Method(s) used in May [interview yr-1]-4th mention"
   METHX117 = "ED-6 Method(s) used in June [interview yr-1] - 1st mention"
   METHX118 = "ED-6 Method(s) used in June [interview yr-1]-2nd mention"
   METHX119 = "ED-6 Method(s) used in June [interview yr-1] - 3rd mention"
   METHX120 = "ED-6 Method(s) used in June [interview yr-1]-4th mention"
   METHX121 = "ED-6 Method(s) used in July [interview yr-1] - 1st mention"
   METHX122 = "ED-6 Method(s) used in July [interview yr-1]-2nd mention"
   METHX123 = "ED-6 Method(s) used in July [interview yr-1] - 3rd mention"
   METHX124 = "ED-6 Method(s) used in July [interview yr-1]-4th mention"
   METHX125 = "ED-6 Method(s) used in Aug [interview yr-1] - 1st mention"
   METHX126 = "ED-6 Method(s) used in Aug [interview yr-1]-2nd mention"
   METHX127 = "ED-6 Method(s) used in Aug [interview yr-1] - 3rd mention"
   METHX128 = "ED-6 Method(s) used in Aug [interview yr-1]-4th mention"
   METHX129 = "ED-6 Method(s) used in Sept [interview yr-1] - 1st mention"
   METHX130 = "ED-6 Method(s) used in Sept [interview yr-1]-2nd mention"
   METHX131 = "ED-6 Method(s) used in Sept [interview yr-1] - 3rd mention"
   METHX132 = "ED-6 Method(s) used in Sept [interview yr-1]-4th mention"
   METHX133 = "ED-6 Method(s) used in Oct [interview yr-1] - 1st mention"
   METHX134 = "ED-6 Method(s) used in Oct [interview yr-1]-2nd mention"
   METHX135 = "ED-6 Method(s) used in Oct [interview yr-1] - 3rd mention"
   METHX136 = "ED-6 Method(s) used in Oct [interview yr-1]-4th mention"
   METHX137 = "ED-6 Method(s) used in Nov [interview yr-1] - 1st mention"
   METHX138 = "ED-6 Method(s) used in Nov [interview yr-1]-2nd mention"
   METHX139 = "ED-6 Method(s) used in Nov [interview yr-1] - 3rd mention"
   METHX140 = "ED-6 Method(s) used in Nov [interview yr-1]-4th mention"
   METHX141 = "ED-6 Method(s) used in Dec [interview yr-1] - 1st mention"
   METHX142 = "ED-6 Method(s) used in Dec [interview yr-1]-2nd mention"
   METHX143 = "ED-6 Method(s) used in Dec [interview yr-1] - 3rd mention"
   METHX144 = "ED-6 Method(s) used in Dec [interview yr-1]-4th mention"
   METHX145 = "ED-6 Method(s) used in Jan [interview yr] - 1st mention"
   METHX146 = "ED-6 Method(s) used in Jan [interview yr]-2nd mention"
   METHX147 = "ED-6 Method(s) used in Jan [interview yr]-3rd mention"
   METHX148 = "ED-6 Method(s) used in Jan [interview yr]-4th mention"
   METHX149 = "ED-6 Method(s) used in Feb [interview yr] - 1st mention"
   METHX150 = "ED-6 Method(s) used in Feb [interview yr]-2nd mention"
   METHX151 = "ED-6 Method(s) used in Feb [interview yr] - 3rd mention"
   METHX152 = "ED-6 Method(s) used in Feb [interview yr]-4th mention"
   METHX153 = "ED-6 Method(s) used in March [interview yr] - 1st mention"
   METHX154 = "ED-6 Method(s) used in March [interview yr]-2nd mention"
   METHX155 = "ED-6 Method(s) used in March [interview yr] - 3rd mention"
   METHX156 = "ED-6 Method(s) used in March [interview yr]-4th mention"
   METHX157 = "ED-6 Method(s) used in April [interview yr] - 1st mention"
   METHX158 = "ED-6 Method(s) used in April [interview yr]-2nd mention"
   METHX159 = "ED-6 Method(s) used in April [interview yr] - 3rd mention"
   METHX160 = "ED-6 Method(s) used in April [interview yr]-4th mention"
   METHX161 = "ED-6 Method(s) used in May [interview yr] - 1st mention"
   METHX162 = "ED-6 Method(s) used in May [interview yr]-2nd mention"
   METHX163 = "ED-6 Method(s) used in May [interview yr] - 3rd mention"
   METHX164 = "ED-6 Method(s) used in May [interview yr]-4th mention"
   METHX165 = "ED-6 Method(s) used in June [interview yr] - 1st mention"
   METHX166 = "ED-6 Method(s) used in June [interview yr]-2nd mention"
   METHX167 = "ED-6 Method(s) used in June [interview yr] - 3rd mention"
   METHX168 = "ED-6 Method(s) used in June [interview yr]-4th mention"
   METHX169 = "ED-6 Method(s) used in July [interview yr] - 1st mention"
   METHX170 = "ED-6 Method(s) used in July [interview yr]-2nd mention"
   METHX171 = "ED-6 Method(s) used in July [interview yr] - 3rd mention"
   METHX172 = "ED-6 Method(s) used in July [interview yr]-4th mention"
   METHX173 = "ED-6 Method(s) used in Aug [interview yr] - 1st mention"
   METHX174 = "ED-6 Method(s) used in Aug [interview yr]-2nd mention"
   METHX175 = "ED-6 Method(s) used in Aug [interview yr] - 3rd mention"
   METHX176 = "ED-6 Method(s) used in Aug [interview yr]-4th mention"
   METHX177 = "ED-6 Method(s) used in Sept [interview yr] - 1st mention"
   METHX178 = "ED-6 Method(s) used in Sept [interview yr]-2nd mention"
   METHX179 = "ED-6 Method(s) used in Sept [interview yr] - 3rd mention"
   METHX180 = "ED-6 Method(s) used in Sept [interview yr]-4th mention"
   METHX181 = "ED-6 Method(s) used in Oct [interview yr] - 1st mention"
   METHX182 = "ED-6 Method(s) used in Oct [interview yr]-2nd mention"
   METHX183 = "ED-6 Method(s) used in Oct [interview yr] - 3rd mention"
   METHX184 = "ED-6 Method(s) used in Oct [interview yr]-4th mention"
   METHX185 = "ED-6 Method(s) used in Nov [interview yr] - 1st mention"
   METHX186 = "ED-6 Method(s) used in Nov [interview yr]-2nd mention"
   METHX187 = "ED-6 Method(s) used in Nov [interview yr] - 3rd mention"
   METHX188 = "ED-6 Method(s) used in Nov [interview yr]-4th mention"
   METHX189 = "ED-6 Method(s) used in Dec [interview yr] - 1st mention"
   METHX190 = "ED-6 Method(s) used in Dec [interview yr]-2nd mention"
   METHX191 = "ED-6 Method(s) used in Dec [interview yr] - 3rd mention"
   METHX192 = "ED-6 Method(s) used in Dec [interview yr]-4th mention"
   CMMHCALX1 = "Century month of Jan [interview yr-3] in the method history calendar"
   CMMHCALX2 = "Century month of  Feb [interview yr-3] in the method history calendar"
   CMMHCALX3 = "Century month of  March [interview yr-3] in the method history calendar"
   CMMHCALX4 = "Century month of  April [interview yr-3] in the method history calendar"
   CMMHCALX5 = "Century month of  May [interview yr-3] in the method history calendar"
   CMMHCALX6 = "Century month of  June [interview yr-3] in the method history calendar"
   CMMHCALX7 = "Century month of  July [interview yr-3] in the method history calendar"
   CMMHCALX8 = "Century month of  Aug [interview yr-3] in the method history calendar"
   CMMHCALX9 = "Century month of  Sept [interview yr-3] in the method history calendar"
   CMMHCALX10 = "Century month of  Oct [interview yr-3] in the method history calendar"
   CMMHCALX11 = "Century month of  Nov [interview yr-3] in the method history calendar"
   CMMHCALX12 = "Century month of  Dec [interview yr-3] in the method history calendar"
   CMMHCALX13 = "Century month of Jan [interview yr-2] in the method history calendar"
   CMMHCALX14 = "Century month of  Feb [interview yr-2] in the method history calendar"
   CMMHCALX15 = "Century month of  March [interview yr-2] in the method history calendar"
   CMMHCALX16 = "Century month of  April [interview yr-2] in the method history calendar"
   CMMHCALX17 = "Century month of  May [interview yr-2] in the method history calendar"
   CMMHCALX18 = "Century month of  June [interview yr-2] in the method history calendar"
   CMMHCALX19 = "Century month of  July [interview yr-2] in the method history calendar"
   CMMHCALX20 = "Century month of  Aug [interview yr-2] in the method history calendar"
   CMMHCALX21 = "Century month of  Sept [interview yr-2] in the method history calendar"
   CMMHCALX22 = "Century month of  Oct [interview yr-2] in the method history calendar"
   CMMHCALX23 = "Century month of  Nov [interview yr-2] in the method history calendar"
   CMMHCALX24 = "Century month of  Dec [interview yr-2] in the method history calendar"
   CMMHCALX25 = "Century month of Jan [interview yr-1] in the method history calendar"
   CMMHCALX26 = "Century month of  Feb [interview yr-1] in the method history calendar"
   CMMHCALX27 = "Century month of  March [interview yr-1] in the method history calendar"
   CMMHCALX28 = "Century month of  April [interview yr-1] in the method history calendar"
   CMMHCALX29 = "Century month of  May [interview yr-1] in the method history calendar"
   CMMHCALX30 = "Century month of  June [interview yr-1] in the method history calendar"
   CMMHCALX31 = "Century month of  July [interview yr-1] in the method history calendar"
   CMMHCALX32 = "Century month of  Aug [interview yr-1] in the method history calendar"
   CMMHCALX33 = "Century month of  Sept [interview yr-1] in the method history calendar"
   CMMHCALX34 = "Century month of  Oct [interview yr-1] in the method history calendar"
   CMMHCALX35 = "Century month of  Nov [interview yr-1] in the method history calendar"
   CMMHCALX36 = "Century month of  Dec [interview yr-1] in the method history calendar"
   CMMHCALX37 = "Century month of Jan [interview yr] in the method history calendar"
   CMMHCALX38 = "Century month of  Feb [interview yr] in the method history calendar"
   CMMHCALX39 = "Century month of  March [interview yr] in the method history calendar"
   CMMHCALX40 = "Century month of  April [interview yr] in the method history calendar"
   CMMHCALX41 = "Century month of  May [interview yr] in the method history calendar"
   CMMHCALX42 = "Century month of  June [interview yr] in the method history calendar"
   CMMHCALX43 = "Century month of  July [interview yr] in the method history calendar"
   CMMHCALX44 = "Century month of  Aug [interview yr] in the method history calendar"
   CMMHCALX45 = "Century month of  Sept [interview yr] in the method history calendar"
   CMMHCALX46 = "Century month of  Oct [interview yr] in the method history calendar"
   CMMHCALX47 = "Century month of  Nov [interview yr] in the method history calendar"
   CMMHCALX48 = "Century month of  Dec [interview yr] in the method history calendar"
   NUMMULTX1 = "Number of Methods Reported in Jan [interview yr-3]"
   NUMMULTX2 = "Number of Methods Reported in Feb [interview yr-3]"
   NUMMULTX3 = "Number of Methods Reported in March [interview yr-3]"
   NUMMULTX4 = "Number of Methods Reported in April [interview yr-3]"
   NUMMULTX5 = "Number of Methods Reported in May [interview yr-3]"
   NUMMULTX6 = "Number of Methods Reported in June [interview yr-3]"
   NUMMULTX7 = "Number of Methods Reported in July [interview yr-3]"
   NUMMULTX8 = "Number of Methods Reported in Aug [interview yr-3]"
   NUMMULTX9 = "Number of Methods Reported in Sept [interview yr-3]"
   NUMMULTX10 = "Number of Methods Reported in Oct [interview yr-3]"
   NUMMULTX11 = "Number of Methods Reported in Nov [interview yr-3]"
   NUMMULTX12 = "Number of Methods Reported in Dec [interview yr-3]"
   NUMMULTX13 = "Number of Methods Reported in Jan [interview yr-2]"
   NUMMULTX14 = "Number of Methods Reported in Feb [interview yr-2]"
   NUMMULTX15 = "Number of Methods Reported in March [interview yr-2]"
   NUMMULTX16 = "Number of Methods Reported in April [interview yr-2]"
   NUMMULTX17 = "Number of Methods Reported in May [interview yr-2]"
   NUMMULTX18 = "Number of Methods Reported in June [interview yr-2]"
   NUMMULTX19 = "Number of Methods Reported in July [interview yr-2]"
   NUMMULTX20 = "Number of Methods Reported in Aug [interview yr-2]"
   NUMMULTX21 = "Number of Methods Reported in Sept [interview yr-2]"
   NUMMULTX22 = "Number of Methods Reported in Oct [interview yr-2]"
   NUMMULTX23 = "Number of Methods Reported in Nov [interview yr-2]"
   NUMMULTX24 = "Number of Methods Reported in Dec [interview yr-2]"
   NUMMULTX25 = "Number of Methods Reported in Jan [interview yr-1]"
   NUMMULTX26 = "Number of Methods Reported in Feb [interview yr-1]"
   NUMMULTX27 = "Number of Methods Reported in March [interview yr-1]"
   NUMMULTX28 = "Number of Methods Reported in April [interview yr-1]"
   NUMMULTX29 = "Number of Methods Reported in May [interview yr-1]"
   NUMMULTX30 = "Number of Methods Reported in June [interview yr-1]"
   NUMMULTX31 = "Number of Methods Reported in July [interview yr-1]"
   NUMMULTX32 = "Number of Methods Reported in Aug [interview yr-1]"
   NUMMULTX33 = "Number of Methods Reported in Sept [interview yr-1]"
   NUMMULTX34 = "Number of Methods Reported in Oct [interview yr-1]"
   NUMMULTX35 = "Number of Methods Reported in Nov [interview yr-1]"
   NUMMULTX36 = "Number of Methods Reported in Dec [interview yr-1]"
   NUMMULTX37 = "Number of Methods Reported in Jan [interview yr]"
   NUMMULTX38 = "Number of Methods Reported in Feb [interview yr]"
   NUMMULTX39 = "Number of Methods Reported in March [interview yr]"
   NUMMULTX40 = "Number of Methods Reported in April [interview yr]"
   NUMMULTX41 = "Number of Methods Reported in May [interview yr]"
   NUMMULTX42 = "Number of Methods Reported in June [interview yr]"
   NUMMULTX43 = "Number of Methods Reported in July [interview yr]"
   NUMMULTX44 = "Number of Methods Reported in Aug [interview yr]"
   NUMMULTX45 = "Number of Methods Reported in Sept [interview yr]"
   NUMMULTX46 = "Number of Methods Reported in Oct [interview yr]"
   NUMMULTX47 = "Number of Methods Reported in Nov [interview yr]"
   NUMMULTX48 = "Number of Methods Reported in Dec [interview yr]"
   SALMX1 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Jan [interview yr-3]"
   SALMX2 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Feb [interview yr-3]"
   SALMX3 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST -Mar [interview yr-3]"
   SALMX4 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - April [interview yr-3]"
   SALMX5 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - May [interview yr-3]"
   SALMX6 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - June [interview yr-3]"
   SALMX7 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - July [interview yr-3]"
   SALMX8 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Aug [interview yr-3]"
   SALMX9 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Sept [interview yr-3]"
   SALMX10 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Oct [interview yr-3]"
   SALMX11 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Nov [interview yr-3]"
   SALMX12 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Dec [interview yr-3]"
   SALMX13 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Jan [interview yr-2]"
   SALMX14 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Feb [interview yr-2]"
   SALMX15 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Mar [interview yr-2]"
   SALMX16 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - April [interview yr-2]"
   SALMX17 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - May [interview yr-2]"
   SALMX18 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - June [interview yr-2]"
   SALMX19 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - July [interview yr-2]"
   SALMX20 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Aug [interview yr-2]"
   SALMX21 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Sept [interview yr-2]"
   SALMX22 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Oct [interview yr-2]"
   SALMX23 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Nov [interview yr-2]"
   SALMX24 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Dec [interview yr-2]"
   SALMX25 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Jan [interview yr-1]"
   SALMX26 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Feb [interview yr-1]"
   SALMX27 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Mar [interview yr-1]"
   SALMX28 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - April [interview yr-1]"
   SALMX29 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - May [interview yr-1]"
   SALMX30 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - June [interview yr-1]"
   SALMX31 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - July [interview yr-1]"
   SALMX32 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Aug [interview yr-1]"
   SALMX33 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Sept [interview yr-1]"
   SALMX34 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Oct [interview yr-1]"
   SALMX35 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Nov [interview yr-1]"
   SALMX36 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Dec [interview yr-1]"
   SALMX37 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Jan [interview yr]"
   SALMX38 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Feb [interview yr]"
   SALMX39 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Mar [interview yr]"
   SALMX40 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - April [interview yr]"
   SALMX41 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - May [interview yr]"
   SALMX42 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - June [interview yr]"
   SALMX43 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - July [interview yr]"
   SALMX44 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Aug [interview yr]"
   SALMX45 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Sept [interview yr]"
   SALMX46 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Oct [interview yr]"
   SALMX47 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Nov [interview yr]"
   SALMX48 = "Flag indicating whether 'same as last month'  was original answer to ED-6 METHHIST - Dec [interview yr]"
   SAYX1 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Jan [interview yr-3]"
   SAYX2 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Feb [interview yr-3]"
   SAYX3 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Mar [interview yr-3]"
   SAYX4 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - April [interview yr-3]"
   SAYX5 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - May [interview yr-3]"
   SAYX6 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - June [interview yr-3]"
   SAYX7 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - July [interview yr-3]"
   SAYX8 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Aug [interview yr-3]"
   SAYX9 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Sept [interview yr-3]"
   SAYX10 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Oct [interview yr-3]"
   SAYX11 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Nov [interview yr-3]"
   SAYX12 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Dec [interview yr-3]"
   SAYX13 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Jan [interview yr-2]"
   SAYX14 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Feb [interview yr-2]"
   SAYX15 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Mar [interview yr-2]"
   SAYX16 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - April [interview yr-2]"
   SAYX17 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - May [interview yr-2]"
   SAYX18 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - June [interview yr-2]"
   SAYX19 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - July [interview yr-2]"
   SAYX20 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Aug [interview yr-2]"
   SAYX21 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Sept [interview yr-2]"
   SAYX22 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Oct [interview yr-2]"
   SAYX23 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Nov [interview yr-2]"
   SAYX24 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Dec [interview yr-2]"
   SAYX25 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Jan [interview yr-1]"
   SAYX26 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Feb [interview yr-1]"
   SAYX27 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Mar [interview yr-1]"
   SAYX28 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - April [interview yr-1]"
   SAYX29 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - May [interview yr-1]"
   SAYX30 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - June [interview yr-1]"
   SAYX31 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - July [interview yr-1]"
   SAYX32 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Aug [interview yr-1]"
   SAYX33 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Sept [interview yr-1]"
   SAYX34 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Oct [interview yr-1]"
   SAYX35 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Nov [interview yr-1]"
   SAYX36 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Dec [interview yr-1]"
   SAYX37 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Jan [interview yr]"
   SAYX38 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Feb [interview yr]"
   SAYX39 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Mar [interview yr]"
   SAYX40 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - April [interview yr]"
   SAYX41 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - May [interview yr]"
   SAYX42 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - June [interview yr]"
   SAYX43 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - July [interview yr]"
   SAYX44 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Aug [interview yr]"
   SAYX45 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Sept [interview yr]"
   SAYX46 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Oct [interview yr]"
   SAYX47 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Nov [interview yr]"
   SAYX48 = "Flag indicating whether 'same all year'  was original answer to ED-6 METHHIST - Dec [interview yr]"
   SIMSEQX1 = "ED-10 Use Methods Together or at Different Times - Jan [interview yr-3]"
   SIMSEQX2 = "ED-10 Use Methods Together or at Different Times - Feb [interview yr-3]"
   SIMSEQX3 = "ED-10 Use Methods Together or at Different Times - Mar [interview yr-3]"
   SIMSEQX4 = "ED-10 Use Methods Together or at Different Times - April [interview yr-3]"
   SIMSEQX5 = "ED-10 Use Methods Together or at Different Times - May [interview yr-3]"
   SIMSEQX6 = "ED-10 Use Methods Together or at Different Times - June [interview yr-3]"
   SIMSEQX7 = "ED-10 Use Methods Together or at Different Times - July [interview yr-3]"
   SIMSEQX8 = "ED-10 Use Methods Together or at Different Times - Aug [interview yr-3]"
   SIMSEQX9 = "ED-10 Use Methods Together or at Different Times - Sept [interview yr-3]"
   SIMSEQX10 = "ED-10 Use Methods Together or at Different Times - Oct [interview yr-3]"
   SIMSEQX11 = "ED-10 Use Methods Together or at Different Times - Nov [interview yr-3]"
   SIMSEQX12 = "ED-10 Use Methods Together or at Different Times - Dec [interview yr-3]"
   SIMSEQX13 = "ED-10 Use Methods Together or at Different Times - Jan [interview yr-2]"
   SIMSEQX14 = "ED-10 Use Methods Together or at Different Times - Feb [interview yr-2]"
   SIMSEQX15 = "ED-10 Use Methods Together or at Different Times - Mar [interview yr-2]"
   SIMSEQX16 = "ED-10 Use Methods Together or at Different Times - April [interview yr-2]"
   SIMSEQX17 = "ED-10 Use Methods Together or at Different Times - May [interview yr-2]"
   SIMSEQX18 = "ED-10 Use Methods Together or at Different Times - June [interview yr-2]"
   SIMSEQX19 = "ED-10 Use Methods Together or at Different Times - July [interview yr-2]"
   SIMSEQX20 = "ED-10 Use Methods Together or at Different Times - Aug [interview yr-2]"
   SIMSEQX21 = "ED-10 Use Methods Together or at Different Times - Sept [interview yr-2]"
   SIMSEQX22 = "ED-10 Use Methods Together or at Different Times - Oct [interview yr-2]"
   SIMSEQX23 = "ED-10 Use Methods Together or at Different Times - Nov [interview yr-2]"
   SIMSEQX24 = "ED-10 Use Methods Together or at Different Times - Dec [interview yr-2]"
   SIMSEQX25 = "ED-10 Use Methods Together or at Different Times - Jan [interview yr-1]"
   SIMSEQX26 = "ED-10 Use Methods Together or at Different Times - Feb [interview yr-1]"
   SIMSEQX27 = "ED-10 Use Methods Together or at Different Times - Mar [interview yr-1]"
   SIMSEQX28 = "ED-10 Use Methods Together or at Different Times - April [interview yr-1]"
   SIMSEQX29 = "ED-10 Use Methods Together or at Different Times - May [interview yr-1]"
   SIMSEQX30 = "ED-10 Use Methods Together or at Different Times - June [interview yr-1]"
   SIMSEQX31 = "ED-10 Use Methods Together or at Different Times - July [interview yr-1]"
   SIMSEQX32 = "ED-10 Use Methods Together or at Different Times - Aug [interview yr-1]"
   SIMSEQX33 = "ED-10 Use Methods Together or at Different Times - Sept [interview yr-1]"
   SIMSEQX34 = "ED-10 Use Methods Together or at Different Times - Oct [interview yr-1]"
   SIMSEQX35 = "ED-10 Use Methods Together or at Different Times - Nov [interview yr-1]"
   SIMSEQX36 = "ED-10 Use Methods Together or at Different Times - Dec [interview yr-1]"
   SIMSEQX37 = "ED-10 Use Methods Together or at Different Times - Jan [interview yr]"
   SIMSEQX38 = "ED-10 Use Methods Together or at Different Times - Feb [interview yr]"
   SIMSEQX39 = "ED-10 Use Methods Together or at Different Times - Mar [interview yr]"
   SIMSEQX40 = "ED-10 Use Methods Together or at Different Times - April [interview yr]"
   SIMSEQX41 = "ED-10 Use Methods Together or at Different Times - May [interview yr]"
   SIMSEQX42 = "ED-10 Use Methods Together or at Different Times - June [interview yr]"
   SIMSEQX43 = "ED-10 Use Methods Together or at Different Times - July [interview yr]"
   SIMSEQX44 = "ED-10 Use Methods Together or at Different Times - Aug [interview yr]"
   SIMSEQX45 = "ED-10 Use Methods Together or at Different Times - Sept [interview yr]"
   SIMSEQX46 = "ED-10 Use Methods Together or at Different Times - Oct [interview yr]"
   SIMSEQX47 = "ED-10 Use Methods Together or at Different Times - Nov [interview yr]"
   SIMSEQX48 = "ED-10 Use Methods Together or at Different Times - Dec [interview yr]"
   USELSTP = "EF-1 Use Method at Last Sex with last partner in past 12 months?"
   WYNOLSTP = "EF-1b No BC use with last partner bec you wanted preg?"
   HPLSTP = "EF-1c No BC use with last partner bec he wanted you to become preg?"
   LSTMTHP1 = "EF-2 Method Used at Last Sex with last partner in past 12 mons -1st mention"
   LSTMTHP2 = "EF-2 Method Used at Last Sex with last partner in past 12 mons -2nd mention"
   LSTMTHP3 = "EF-2 Method Used at Last Sex with last partner in past 12 mons -3rd mention"
   LSTMTHP4 = "EF-2 Method Used at Last Sex with last partner in past 12 mons -4th mention"
   LPIUDTYP = "EF-2b Type of IUD used with last partner"
   USEFSTP = "EF-3 Use Method at First Sex with last partner in past 12 months?"
   FSTMTHP1 = "EF-4 Method used at first sex with last partner in past 12 mons -1st mention"
   FSTMTHP2 = "EF-4 Method used at first sex with last partner in past 12 mons -2nd mention"
   FSTMTHP3 = "EF-4 Method used at first sex with last partner in past 12 mons -3rd mention"
   FSTMTHP4 = "EF-4 Method used at first sex with last partner in past 12 mons -4th mention"
   USELSTP2 = "EF-1 Use Method at Last Sex with 2nd-to-last partner in past 12 months?"
   WYNOLSTP2 = "EF-1b No BC use with 2nd-to-last partner bec you wanted preg?"
   HPLSTP2 = "EF-1c No BC use with 2nd-to-last partner bec he wanted you to become preg?"
   LSTMTHP5 = "EF-2 Method Used at Last Sex with 2nd-to-last partner in past 12 mons -1st mention"
   LSTMTHP6 = "EF-2 Method Used at Last Sex with 2nd-to-last partner in past 12 mons -2nd mention"
   LSTMTHP7 = "EF-2 Method Used at Last Sex with 2nd-to-last partner in past 12 mons -3rd mention"
   LSTMTHP8 = "EF-2 Method Used at Last Sex with 2nd-to-last partner in past 12 mons -4th mention"
   LPIUDTYP2 = "EF-2b Type of IUD used with 2nd-to-last partner"
   USEFSTP2 = "EF-3 Use Method at First Sex with 2nd-to-last partner in past 12 months?"
   FSTMTHP5 = "EF-4 Method used at first sex with 2nd-to-last partner in past 12 mons -1st mention"
   FSTMTHP6 = "EF-4 Method used at first sex with 2nd-to-last partner in past 12 mons -2nd mention"
   FSTMTHP7 = "EF-4 Method used at first sex with 2nd-to-last partner in past 12 mons -3rd mention"
   FSTMTHP8 = "EF-4 Method used at first sex with 2nd-to-last partner in past 12 mons -4th mention"
   USELSTP3 = "EF-1 Use Method at Last Sex with 3rd-to-last partner in past 12 months?"
   WYNOLSTP3 = "EF-1b No BC use with 3rd-to-last partner bec you wanted preg?"
   HPLSTP3 = "EF-1c No BC use with 3rd-to-last partner bec he wanted you to become preg?"
   LSTMTHP9 = "EF-2 Method Used at Last Sex with 3rd-to-last partner in past 12 mons - 1st mention"
   LSTMTHP10 = "EF-2 Method Used at Last Sex with 3rd-to-last partner in past 12 mons -2nd mention"
   LSTMTHP11 = "EF-2 Method Used at Last Sex with 3rd-to-last partner in past 12 mons -3rd mention"
   LSTMTHP12 = "EF-2 Method Used at Last Sex with 3rd-to-last partner in past 12 mons -4th mention"
   LPIUDTYP3 = "EF-2b Type of IUD used with 3rd-to-last partner"
   USEFSTP3 = "EF-3 use method at first sex with 3rd-to-last partner in past 12 months?"
   FSTMTHP9 = "EF-4 Method used at first sex with 3rd-to-last partner in past 12 mons -1st mention"
   FSTMTHP10 = "EF-4 Method used at first sex with 3rd-to-last partner in past 12 mons -2nd mention"
   FSTMTHP11 = "EF-4 Method used at first sex with 3rd-to-last partner in past 12 mons -3rd mention"
   FSTMTHP12 = "EF-4 Method used at first sex with 3rd-to-last partner in past 12 mons -4th mention"
   WYNOTUSE = "EH-1 Reason not using birth control because you want to become pregnant?"
   HPPREGQ = "EH-2 Does partner want you to become pregnant?"
   DURTRY_N = "EH-2a How long have you been trying to get pregnant?  (number)"
   DURTRY_P = "EH-2b How long have you been trying to get pregnant? (unit: Mons/Yrs)"
   WHYNOUSING1 = "EH-2c Reason not using birth control (at risk of unint preg) - 1st mention"
   WHYNOUSING2 = "EH-2c Reason not using birth control (at risk of unint preg) - 2nd mention"
   WHYNOUSING3 = "EH-2c Reason not using birth control (at risk of unint preg) - 3rd mention"
   WHYNOUSING4 = "EH-2c Reason not using birth control (at risk of unint preg) - 4th mention"
   WHYNOUSING5 = "EH-2c Reason not using birth control (at risk of unint preg) - 5th mention"
   WHYNOTPG1 = "EH-2cc open-ended response: reason don't think can get preg - 1st"
   WHYNOTPG2 = "EH-2cc open-ended response: reason don't think can get preg - 2nd"
   MAINNOUSE = "EH-2d Main reason not using birth control (at risk of unint preg)"
   YUSEPILL1 = "EJ-1 Reasons for recent pill use-1st mention"
   YUSEPILL2 = "EJ-1 Reasons for recent pill use-2nd mention"
   YUSEPILL3 = "EJ-1 Reasons for recent pill use-3rd mention"
   YUSEPILL4 = "EJ-1 Reasons for recent pill use-4th mention"
   YUSEPILL5 = "EJ-1 Reasons for recent pill use-5th mention"
   YUSEPILL6 = "EJ-1 Reasons for recent pill use-6th mention"
   YUSEPILL7 = "EJ-1 Reasons for recent pill use-7th mention"
   IUDTYPE = "EJ-3 Type of IUD used the last two months"
   YUSEIUD1 = "EJ-3a Reasons for recent IUD use-1st mention"
   YUSEIUD2 = "EJ-3a Reasons for recent IUD use-2nd mention"
   YUSEIUD3 = "EJ-3a Reasons for recent IUD use-3rd mention"
   YUSEIUD4 = "EJ-3a Reasons for recent IUD use-4th mention"
   YUSEIUD5 = "EJ-3a Reasons for recent IUD use-5th mention"
   YUSEIUD6 = "EJ-3a Reasons for recent IUD use-6th mention"
   EKMETHOD = "EKMETHOD - Recently used method chosen for use in EK Series (CURBCPLC-CURBCAMT)"
   CURBCPLC = "EK-1 Source of recent method use"
   NUMPILLS = "EK-2 How many months of pills did you get, last time got some?"
   CURBCPAY1 = "EK-3 How pay for current method?-1st mention"
   CURBCPAY2 = "EK-3 How pay for current method?-2nd mention"
   CURBCPAY3 = "EK-3 How pay for current method?-3rd mention"
   CURBCINS = "EK-4 Have health ins when got curr method?"
   NOUSEINS1 = "EK-5 Why not use health ins to pay for curr method?-1st mention"
   NOUSEINS2 = "EK-5 Why not use health ins to pay for curr method?-2nd mention"
   NOUSEINS3 = "EK-5 Why not use health ins to pay for curr method?-3rd mention"
   CURBCAMT = "EK-6 How much paid for curr method"
   NOCOST1 = "EK-7 If cost not an issue, want to use diff method?"
   NOCOST2 = "EK-8 If cost not an issue, want to use a method?"
   PST4WKSX = "EL-1 # times sex with male in last 4 weeks"
   PSWKCOND1 = "EL-2 Use a condom during sex with male in last 4 wks"
   PSWKCOND2 = "EL-3 How many times use condom during sex last 4 wks"
   COND1BRK = "EL-3a Did condom break"
   COND1OFF = "EL-3b When was the condom used"
   CONDBRFL = "EL-3c How many times use condom break or completely fall off"
   CONDOFF = "EL-3d How many times was the condom put on after or taken off during sex"
   MISSPILL = "EL-3e number of pills missed"
   P12MOCON = "EL-4 How many times used condom during sex last 12 mons"
   PXNOFREQ = "EL-5 How many times used any method during sex last 12 mons"
   BTHCON12 = "FA-1b Rec'd Method Birth Control/Prescription-Last 12 Months"
   MEDTST12 = "FA-1c Rec'd Checkup for Birth Control Last 12 Months"
   BCCNS12 = "FA-1d Rec'd Counseling About Birth Control Last 12 Months"
   STEROP12 = "FA-1e Rec'd sterilizing operation in last 12 months?"
   STCNS12 = "FA-1f Rec'd Counseling re Getting Sterilized Last 12 Months"
   EMCON12 = "FA-1g Rec'd Emergency Contraception/Prescription Last 12 Months"
   ECCNS12 = "FA-1h Rec'd Counseling re Emergency Contraception Last 12 Months"
   NUMMTH12 = "# of BC Methods used (drug/device) Last 12 Months"
   PRGTST12 = "FA-3a Received Pregnancy Test Last 12 Months"
   ABORT12 = "FA-3b Received Abortion Last 12 Months"
   PAP12 = "FA-3c Received Pap Smear Last 12 Months"
   PELVIC12 = "FA-3d Received Pelvic Exam Last 12 Months"
   PRENAT12 = "FA-3e Received Prenatal Care Last 12 Months"
   PARTUM12 = "FA-3f Received Post-Pregnancy Care Last 12 Months"
   STDSVC12 = "FA-3g  Rec'd test for STD Last 12 Months"
   BARRIER1 = "FA-3h Reason Did Not See a Doctor in Last 12 months - 1st mention"
   BARRIER2 = "FA-3h Reason Did Not See a Doctor in Last 12 months - 2nd mention"
   BARRIER3 = "FA-3h Reason Did Not See a Doctor in Last 12 months - 3rd mention"
   BARRIER4 = "FA-3h Reason Did Not See a Doctor in Last 12 months - 4th mention"
   BARRIER5 = "FA-3h Reason Did Not See a Doctor in Last 12 months - 5th mention"
   BARRIER6 = "FA-3h Reason Did Not See a Doctor in Last 12 months - 6th mention"
   NUMSVC12 = "# of Services Received in Last 12 Months"
   NUMBCVIS = "FA-4 Received Services Last 12 Months at 1 or More Visits"
   BC12PLCX = "FA-5 Where R Rec'd all services in one visit"
   BC12PLCX2 = "FA-5 Where R Rec'd Method of Birth Control/Prescription in Last 12 Months"
   BC12PLCX3 = "FA-5 Where R Rec'd Check-up for Birth Control in Last 12 Months"
   BC12PLCX4 = "FA-5 Where R Rec'd Counseling about Birth Control in Last 12 Months"
   BC12PLCX5 = "FA-5 Where R Rec'd sterilizing operation in Last 12 Months"
   BC12PLCX6 = "FA-5 Where R Rec'd Counseling on Getting Sterilized in Last 12 Months"
   BC12PLCX7 = "FA-5 Where R Rec'd Emergency Contraception in Last 12 Months"
   BC12PLCX8 = "FA-5 Where R Rec'd Counseling on Emerg. Contraception in Last 12 Months"
   BC12PLCX9 = "FA-5 Where R Rec'd Pregnancy Test in Last 12 Months"
   BC12PLCX10 = "FA-5 Where R Rec'd Abortion in Last 12 Months"
   BC12PLCX11 = "FA-5 Where R Rec'd Pap Smear in Last 12 Months"
   BC12PLCX12 = "FA-5 Where R Rec'd Pelvic Exam in Last 12 Months"
   BC12PLCX13 = "FA-5 Where R Rec'd Prenatal Care in Last 12 Months"
   BC12PLCX14 = "FA-5 Where R Rec'd Post-pregnancy care in Last 12 Months"
   BC12PLCX15 = "FA-5 Where R Rec'd testing for STD in Last 12 Months"
   IDCLINIC = "Number of times identified a clinic"
   TALKPROV_1 = "FA-5a Talked to provider in the past 12 months about specific topics: 1st mention"
   TALKPROV_2 = "FA-5a Talked to provider in the past 12 months about specific topics: 2nd mention"
   TALKPROV_3 = "FA-5a Talked to provider in the past 12 months about specific topics: 3rd mention"
   WHYPSTD = "FA-5e Main reason chose place for STD test"
   WHYNOSTD = "FA-5e1 Main reason no STD test in past 12 months"
   BCCLARC = "FA-5f When info about birth control whether provider talked about LARC"
   BC12PAYX1 = "Fa-6 Way Bill Was Paid-all services in one visit, 1st method"
   BC12PAYX2 = "Fa-6 Way Bill Was Paid-all services in one visit, 2nd method"
   BC12PAYX3 = "Fa-6 Way Bill Was Paid-all services in one visit, 3rd method"
   BC12PAYX4 = "Fa-6 Way Bill Was Paid-all services in one visit, 4th method"
   BC12PAYX7 = "Fa-6 Way Bill Was Paid-method BC/prescription, 1st method"
   BC12PAYX8 = "Fa-6 Way Bill Was Paid-method BC/prescription, 2nd method"
   BC12PAYX9 = "Fa-6 Way Bill Was Paid-method BC/prescription, 3rd method"
   BC12PAYX10 = "Fa-6 Way Bill Was Paid-method BC/prescription, 4th method"
   BC12PAYX13 = "Fa-6 Way Bill Was Paid-check up for BC, 1st method"
   BC12PAYX14 = "Fa-6 Way Bill Was Paid-check up for BC, 2nd method"
   BC12PAYX15 = "Fa-6 Way Bill Was Paid-check up for BC, 3rd method"
   BC12PAYX16 = "Fa-6 Way Bill Was Paid-check up for BC, 4th method"
   BC12PAYX19 = "Fa-6 Way Bill Was Paid-counseling about BC, 1st method"
   BC12PAYX20 = "Fa-6 Way Bill Was Paid-counseling about BC, 2nd  method"
   BC12PAYX21 = "Fa-6 Way Bill Was Paid-counseling about BC, 3rd method"
   BC12PAYX22 = "Fa-6 Way Bill Was Paid-counseling about BC, 4th method"
   BC12PAYX25 = "Fa-6 Ways Bill Was paid-sterilizing operation, 1st method"
   BC12PAYX26 = "Fa-6 Ways Bill Was paid-sterilizing operation, 2nd method"
   BC12PAYX27 = "Fa-6 Ways Bill Was paid-sterilizing operation, 3rd method"
   BC12PAYX28 = "Fa-6 Ways Bill Was paid-sterilizing operation, 4th method"
   BC12PAYX31 = "Fa-6 Way Bill Was Paid-counseling sterilization,  1st method"
   BC12PAYX32 = "Fa-6 Way Bill Was Paid-counseling sterilization,  2nd method"
   BC12PAYX33 = "Fa-6 Way Bill Was Paid-counseling sterilization,  3rd method"
   BC12PAYX34 = "Fa-6 Way Bill Was Paid-counseling sterilization,  4th method"
   BC12PAYX37 = "Fa-6 Way Bill Was Paid-EC/prescription, 1st method"
   BC12PAYX38 = "Fa-6 Way Bill Was Paid-EC/prescription, 2nd method"
   BC12PAYX39 = "Fa-6 Way Bill Was Paid-EC/prescription, 3rd method"
   BC12PAYX40 = "Fa-6 Way Bill Was Paid-EC/prescription, 4th method"
   BC12PAYX43 = "Fa-6 Way Bill Was Paid-counseling EC, 1st method"
   BC12PAYX44 = "Fa-6 Way Bill Was Paid-counseling EC, 2nd method"
   BC12PAYX45 = "Fa-6 Way Bill Was Paid-counseling EC, 3rd method"
   BC12PAYX46 = "Fa-6 Way Bill Was Paid-counseling EC, 4th method"
   BC12PAYX49 = "Fa-6 Way Bill Was Paid-pregnancy test, 1st method"
   BC12PAYX50 = "Fa-6 Way Bill Was Paid-pregnancy test, 2nd method"
   BC12PAYX51 = "Fa-6 Way Bill Was Paid-pregnancy test, 3rd method"
   BC12PAYX52 = "Fa-6 Way Bill Was Paid-pregnancy test, 4th method"
   BC12PAYX55 = "Fa-6 Way Bill Was Paid-abortion, 1st method"
   BC12PAYX56 = "Fa-6 Way Bill Was Paid-abortion, 2nd method"
   BC12PAYX57 = "Fa-6 Way Bill Was Paid-abortion, 3rd method"
   BC12PAYX58 = "Fa-6 Way Bill Was Paid-abortion, 4th method"
   BC12PAYX61 = "Fa-6 Way Bill Was Paid-pap smear, 1st method"
   BC12PAYX62 = "Fa-6 Way Bill Was Paid-pap smear, 2nd method"
   BC12PAYX63 = "Fa-6 Way Bill Was Paid-pap smear, 3rd method"
   BC12PAYX64 = "Fa-6 Way Bill Was Paid-pap smear, 4th method"
   BC12PAYX67 = "Fa-6 Way Bill Was Paid-pelvic exam, 1st method"
   BC12PAYX68 = "Fa-6 Way Bill Was Paid-pelvic exam, 2nd method"
   BC12PAYX69 = "Fa-6 Way Bill Was Paid-pelvic exam, 3rd method"
   BC12PAYX70 = "Fa-6 Way Bill Was Paid-pelvic exam, 4th method"
   BC12PAYX73 = "Fa-6 Way Bill Was Paid-prenatal, 1st method"
   BC12PAYX74 = "Fa-6 Way Bill Was Paid-prenatal, 2nd method"
   BC12PAYX75 = "Fa-6 Way Bill Was Paid-prenatal, 3rd method"
   BC12PAYX76 = "Fa-6 Way Bill Was Paid-prenatal, 4th method"
   BC12PAYX79 = "Fa-6 Way Bill Was Paid-post-pregnancy, 1st method"
   BC12PAYX80 = "Fa-6 Way Bill Was Paid-post-pregnancy, 2nd method"
   BC12PAYX81 = "Fa-6 Way Bill Was Paid-post-pregnancy, 3rd method"
   BC12PAYX82 = "Fa-6 Way Bill Was Paid-post-pregnancy, 4th method"
   BC12PAYX85 = "Fa-6 Way Bill Was Paid-testing for std, 1st method"
   BC12PAYX86 = "Fa-6 Way Bill Was Paid-testing for std, 2nd method"
   BC12PAYX87 = "Fa-6 Way Bill Was Paid-testing for std, 3rd method"
   BC12PAYX88 = "Fa-6 Way Bill Was Paid-testing for std, 4th method"
   REGCAR12_F_01 = "FA-9 Clinic Rec'd all services in one visit"
   REGCAR12_F_02 = "FA-9 Clinic Rec'd Method BC or Prescription was R's Reg Place"
   REGCAR12_F_03 = "FA-9 Clinic Rec'd Check-up for Birth Control was R's Reg Place"
   REGCAR12_F_04 = "FA-9 Clinic Rec'd Counseling for BC was R's Reg Place"
   REGCAR12_F_05 = "FA-9 Clinic Rec'd sterilizing operation was R's Regular Place"
   REGCAR12_F_06 = "FA-9 Clinic Rec'd Counseling for Sterilization was R's Reg Place"
   REGCAR12_F_07 = "FA-9 Clinic Rec'd EC /Prescription was R's Reg Place"
   REGCAR12_F_08 = "FA-9 Clinic Rec'd Counseling on EC was R's Regr Place"
   REGCAR12_F_09 = "FA-9 Clinic Rec'd pregnancy test was R's Regr Place"
   REGCAR12_F_10 = "FA-9 Clinic Rec'd an Abortion was R's Regular Place"
   REGCAR12_F_11 = "FA-9 Clinic Rec'd a Pap Smear was R's Regular Place"
   REGCAR12_F_12 = "FA-9 Clinic Rec'd Pelvic exam was R's Regular Place"
   REGCAR12_F_13 = "FA-9 Clinic Rec'd Prenatal Care was R's Regular Place"
   REGCAR12_F_14 = "FA-9 Clinic Rec'd Post-Pregnancy Care  was R's Regular Place"
   REGCAR12_F_15 = "FA-9 Clinic Rec'd testing for STD was R's Reg Place"
   EVERFPC = "FC-1 Has R ever Visited Clinic for Medical/BC Service"
   KNDMDHLP01 = "FC-2 Medical Services R Received at Clinic -1st mention"
   KNDMDHLP02 = "FC-2 Medical Services R Received at Clinic -2nd mention"
   KNDMDHLP03 = "FC-2 Medical Services R Received at Clinic -3rd mention"
   KNDMDHLP04 = "FC-2 Medical Services R Received at Clinic -4th mention"
   KNDMDHLP05 = "FC-2 Medical Services R Received at Clinic -5th mention"
   KNDMDHLP06 = "FC-2 Medical Services R Received at Clinic -6th mention"
   KNDMDHLP07 = "FC-2 Medical Services R Received at Clinic -7th mention"
   KNDMDHLP08 = "FC-2 Medical Services R Received at Clinic -8th mention"
   KNDMDHLP09 = "FC-2 Medical Services R Received at Clinic -9th mention"
   LASTPAP = "FD-2 When was last Pap test"
   MREASPAP = "FD-3 Main reason for last Pap test"
   AGEFPAP = "FD-4 Age at first Pap test"
   AGEFPAP2 = "FD-4a Was R younger than 18, 18-21, 22-29, or 30 or older at first Pap test"
   ABNPAP3 = "FD-5 Has R had abnormal Pap test in the last 3 years"
   INTPAP = "FD-6 How often R thinks she will need a Pap test"
   PELWPAP = "FE-1 Was pelvic exam done at same time as Pap test"
   LASTPEL = "FE-2 When was last pelvic exam"
   MREASPEL = "FE-3 Main reason for last pelvic exam"
   AGEFPEL = "FE-4 Age at first pelvic exam"
   AGEPEL2 = "FE-4a Was R younger than 18, 18-21, 22-29, or 30 or older at first pelvic exam"
   INTPEL = "FE-5 How often R thinks she will need a pelvic exam"
   EVHPVTST = "FF-2 Ever had HPV test"
   HPVWPAP = "FF-3 Was HPV test done at same time as Pap test"
   LASTHPV = "FF-3c When was last HPV test"
   MREASHPV = "FF-4 Main reason for last HPV test"
   AGEFHPV = "FF-5 Age at first HPV test"
   AGEHPV2 = "FF-5a Was R younger than 18, 18-21, 22-29, or 30 or older at first HPV test"
   INTHPV = "FF-6 How often R thinks she will need an HPV test"
   ASKSMOKE = "FG-2 Provider asked R whether smoke cigarettes or other tobacco"
   ASKPREG = "FG-3 Provider asked R whether wanted to get pregnant"
   ASKFOLIC = "FG-4 Provider advised R to take vitamin with folic acid"
   TALKDM = "FG-5 At visits last 12 months doctor talked using condom & female method"
   PROVRESP = "FH-1 In past 12 months rate provider on respecting you as a person"
   PROVSAYBC = "FH-2 In past 12 months rate provider on letting you say what mattered re birth control"
   PROVPREBC = "FH-3 In past 12 months rate provider on taking preference seriously re birth control"
   PROVINFOBC = "FH-4 In past 12 months rate provider on giving enough info to make best decision re birth control"
   RWANT = "GA-1 R Wants A(another) Baby Some Time?"
   PROBWANT = "GA-1a R Prob Wants/does not want A(another) Baby Some Time?"
   PWANT = "GA-2 R's H/P Wants A(another) Baby Some Time?"
   JINTEND = "GB-1 R & H/P Intend to have A(another) Baby Some Time?"
   JSUREINT = "GB-2 How sure R & H/P will/will not have A(another) Baby?"
   JINTENDN = "GB-3 How Many (More) Babies R and H/P Intend?"
   JEXPECTL = "GB-4 Largest Number of (additional) Babies R and H/P Intend"
   JEXPECTS = "GB-5 Smallest Number of (additional) Babies R and H/P Intend"
   JINTNEXT = "GB-6 When R & H/P expect 1st/next child to be born"
   INTEND = "GC-1 R Intends to Have A(another) Baby Some Time?"
   SUREINT = "GC-2 R How Sure R will/ will not have A(another) baby?"
   INTENDN = "GC-3 Number of (more) Babies R Intends"
   EXPECTL = "GC-4 Largest Number of (additional) Babies R Intends"
   EXPECTS = "GC-5 Smallest Number of (additional) Babies R Intends"
   INTNEXT = "GC-6 When R expects 1st/next child to be born"
   HLPPRG = "HA-1 Ever Received Medical Help to Get Pregnant"
   HOWMANYR = "HA-2 # of Partners with Whom R Sought Medical Help to Get Pregnant"
   SEEKWHO1 = "HA-3 Sought Medical Help to Get Pregnant with Current H/P"
   SEEKWHO2 = "HA-4 Ever Sought Medical Help to Get Pregnant with Current H/P"
   TYPALLPG1 = "HA-5 Infertility Services Received-1st mention"
   TYPALLPG2 = "HA-5 Infertility Services Received-2nd mention"
   TYPALLPG3 = "HA-5 Infertility Services Received-3rd mention"
   TYPALLPG4 = "HA-5 Infertility Services Received-4th mention"
   TYPALLPG5 = "HA-5 Infertility Services Received-5th mention"
   TYPALLPG6 = "HA-5 Infertility Services Received-6th mention"
   WHOTEST = "HA-5a Who had Infertility Testing - R or Partner or Both"
   OVUL12M = "HA-5a2 Ovulation-inducing drugs used within past year"
   WHARTIN = "HA-5b R Inseminated with Partner or Donor Sperm"
   INSEM12M = "HA-5b2 Artificial insemination within past year"
   OTMEDHEP1 = "HA-5c Other Infertility Services Received-1st mention"
   OTMEDHEP2 = "HA-5c Other Infertility Services Received-2nd mention"
   OTMEDHEP3 = "HA-5c Other Infertility Services Received-3rd mention"
   OTMEDHEP4 = "HA-5c Other Infertility Services Received-4th mention"
   INSCOVPG = "HA-6 Infertility Services Covered by Private Insurance"
   FSTHLPPG_M = "HA-7 Month 1st Went for Medical Help To Get Pregnant"
   FSTHLPPG_Y = "HA-7 Year 1st Went for Medical Help To Get Pregnant"
   CMPGVIS1 = "Date (CM) of First Visit for Medical Help to Get Pregnant"
   TRYLONG2 = "HA-8 How Long R Trying To Get Pregnant"
   UNIT_TRYLONG = "HA-8 Unit (months/years) for How Long Trying to Get Pregnant"
   HLPPGNOW = "HA-9 R Currently Pursuing Medical Help to Get Pregnant"
   RCNTPGH_M = "HA-10 Month of Last/Most Recent Visit for Help to Get Pregnant"
   RCNTPGH_Y = "HA-10 Year of Last/Most Recent Visit for Help to Get Pregnant"
   CMPGVISL = "Date (CM) of Last/Most Recent Visit for Medical Help to Get Pregnant"
   NUMVSTPG = "HA-11 # Visits in Last 12 Mos for Medical Help to Get Pregnant"
   PRGVISIT = "# of Visits in Last 12 Months for Medical Help to Get Pregnant"
   HLPMC = "HB-1 Ever Received Medical Help To Prevent Miscarriage"
   TYPALLMC1 = "HB-2 Miscarriage Services-1st Mentioned"
   TYPALLMC2 = "HB-2 Miscarriage Services-2nd Mentioned"
   TYPALLMC3 = "HB-2 Miscarriage Services-3rd Mentioned"
   TYPALLMC4 = "HB-2 Miscarriage Services-4th Mentioned"
   TYPALLMC5 = "HB-2 Miscarriage Services-5th Mentioned"
   MISCNUM = "HB-3 # of Miscarriages at First Visit for Miscarriage Services"
   INFRTPRB1 = "HB-4 Ever Told You Had Any Infertility Problems-1st Mentioned"
   INFRTPRB2 = "HB-4 Ever Told You Had Any Infertility Problems-2nd Mentioned"
   INFRTPRB3 = "HB-4 Ever Told You Had Any Infertility Problems-3rd Mentioned"
   INFRTPRB4 = "HB-4 Ever Told You Had Any Infertility Problems-4th Mentioned"
   INFRTPRB5 = "HB-4 Ever Told You Had Any Infertility Problems-5th Mentioned"
   DUCHFREQ = "HC-1 Last 12 Mos: How Often R Douched"
   PID = "HD-1 Has R Ever Been Treated For PID"
   PIDSYMPT = "HD-2 Was R Having Any Symptoms that Prompted PID Treatment"
   PIDTX = "HD-3 Number of Times R was Treated For PID"
   LSTPIDTX_M = "HD-4 Month R Was Last Treated For PID"
   LSTPIDTX_Y = "HD-4 Year R Was Last Treated For PID"
   CMPIDLST = "Date (CM) of Last/Most Recent PID Treatment"
   DIABETES = "HD-5 Diabetes: Ever Diagnosed"
   GESTDIAB = "HD-6 Nongestational Diabetes: Ever Diagnosed"
   UF = "HD-8 Uterine Fibroids: Ever Diagnosed"
   UFSONO = "HD-8a Fibroids diagnosis confirmed by ultrasound"
   UFCURR = "HD-8b R currently has fibroids"
   UFDIAGNOS = "HD-8c How long ago was R diagnosed with fibroids"
   UFLIMIT = "HD-8d Limitations due to fibroids"
   UFTREAT_1 = "HD-8e Treatment received for fibroids -1st mention"
   UFTREAT_2 = "HD-8e Treatment received for fibroids -2nd mention"
   UFTREAT_3 = "HD-8e Treatment received for fibroids -3rd mention"
   UFTREAT_4 = "HD-8e Treatment received for fibroids -4th mention"
   UFTREAT_5 = "HD-8e Treatment received for fibroids -5th mention"
   UFTREAT_6 = "HD-8e Treatment received for fibroids -6th mention"
   UFTREAT_7 = "HD-8e Treatment received for fibroids -7th mention"
   UFTREAT_8 = "HD-8e Treatment received for fibroids -8th mention"
   ENDO = "HD-9 Endometriosis: Ever Diagnosed"
   ENDOCURR = "HD-9a R currently has endometriosis"
   ENDODIAG = "HD-9b How long ago was R diagnosed with endometriosis"
   ENDOLIM = "HD-9c Limitations due to endometriosis"
   ENDOTREAT_1 = "HD-9d.Treatment received for endometriosis -1st mention"
   ENDOTREAT_2 = "HD-9d.Treatment received for endometriosis -2nd mention"
   ENDOTREAT_3 = "HD-9d.Treatment received for endometriosis -3rd mention"
   ENDOTREAT_4 = "HD-9d.Treatment received for endometriosis -4th mention"
   ENDOTREAT_5 = "HD-9d.Treatment received for endometriosis -5th mention"
   ENDOTREAT_6 = "HD-9d.Treatment received for endometriosis -6th mention"
   ENDOTREAT_7 = "HD-9d.Treatment received for endometriosis -7th mention"
   ENDOTREAT_8 = "HD-9d.Treatment received for endometriosis -8th mention"
   OVUPROB = "HD-10 Ovulation/Menstrual Problems: Ever Diagnosed"
   PCOS = "HD-10b R ever told she has polycystic ovarian syndrome"
   PCOSSYMP_1 = "HD-10c Basis of R s PCOS diagnosis - 1st mention"
   PCOSSYMP_2 = "HD-10c Basis of R s PCOS diagnosis - 2nd mention"
   PCOSSYMP_3 = "HD-10c Basis of R s PCOS diagnosis - 3rd mention"
   PCOSSYMP_4 = "HD-10c Basis of R s PCOS diagnosis - 4th mention"
   PCOSSYMP_5 = "HD-10c Basis of R s PCOS diagnosis - 5th mention"
   PCOSSYMP_6 = "HD-10c Basis of R s PCOS diagnosis - 6th mention"
   DEAF = "HD-11 R has any serious difficulty hearing"
   BLIND = "HD-12 R has any serious difficulty seeing"
   DIFDECIDE = "HD-13 R has any serious difficulty w/memory or decision-making"
   DIFWALK = "HD-14 R has any serious difficulty walking or climbing stairs"
   DIFDRESS = "HD-15 R has any serious difficulty dressing or bathing"
   DIFOUT = "HD-16 R has any difficulty doing errands alone due to phys/mental/emotional conditions"
   EVRCANCER = "HD-17 R has ever had cancer"
   AGECANCER = "HD-17a Age when R was first told she had cancer"
   PRECANCER = "HD-17c Type of cervical cancer diagnosis R had"
   MAMMOG = "HD-18 R has ever had a mammogram"
   AGEMAMM1 = "HD-18a Age when R first had a mammogram"
   REASMAMM1 = "HD-18b Main reason for R's first mammogram"
   CLINEXAM = "HD-18c Ever had a clinical breast exam"
   FAMHYST = "HD-19 R has family history of breast cancer"
   MOMRISK70 = "HD-20a Breast cancer risk if mother was diagnosed at 70"
   MOMRISK40 = "HD-20b Breast cancer risk if mother was diagnosed at 40"
   ALCORISK = "HD-22 R's assessment of breast cancer risk due to alcohol consumption"
   BFEEDRISK = "HD-22a R's assessment of breast cancer risk due to breastfeeding"
   DONBLOOD = "HE-1 Has R Ever Donated Blood or Blood Products"
   HIVTEST = "HE-2 Any HIV Test Outside of Blood Donation"
   NOHIVTST = "HE-2b R's main reason for never having had an HIV test"
   WHENHIV_M = "HE-3 Month of Most Recent HIV Test"
   WHENHIV_Y = "HE-3 Year of Most Recent HIV Test"
   CMHIVTST = "Date (CM) of Last (or Most Recent) HIV Test"
   HIVTSTYR = "HE-3b Has R had HIV test in last 12 months"
   HIVRESULT = "HE-3d Whether R found out results of last HIV test"
   WHYNOGET = "HE-3e Main reason why R did not get results of last HIV test"
   PLCHIV = "HE-4 Place Where R Had Last HIV Test"
   RHHIVT1 = "HE-4j Used a rapid home HIV test"
   RHHIVT21 = "HE-4k Reasons for rapid home HIV test - 1st mentioned"
   RHHIVT22 = "HE-4k Reasons for rapid home HIV test - 2nd mentioned"
   HIVTST = "HE-5 Reason For This Last HIV Test-1st mention"
   PREPHIV = "HE-5c Has R heard about PrEP medications"
   TALKDOCT = "HE-6 Has a doctor ever talked with R about HIV/AIDS"
   AIDSTALK01 = "HE-7 HIV Topics Covered in Discussion with Doctor/Provider-1st mention"
   AIDSTALK02 = "HE-7 HIV Topics Covered in Discussion with Doctor/Provider-2nd mention"
   AIDSTALK03 = "HE-7 HIV Topics Covered in Discussion with Doctor/Provider-3rd mention"
   AIDSTALK04 = "HE-7 HIV Topics Covered in Discussion with Doctor/Provider-4th mention"
   AIDSTALK05 = "HE-7 HIV Topics Covered in Discussion with Doctor/Provider-5th mention"
   AIDSTALK06 = "HE-7 HIV Topics Covered in Discussion with Doctor/Provider-6th mention"
   AIDSTALK07 = "HE-7 HIV Topics Covered in Discussion with Doctor/Provider-7th mention"
   AIDSTALK08 = "HE-7 HIV Topics Covered in Discussion with Doctor/Provider-8th mention"
   AIDSTALK09 = "HE-7 HIV Topics Covered in Discussion with Doctor/Provider-9th mention"
   AIDSTALK10 = "HE-7 HIV Topics Covered in Discussion with Doctor/Provider-10th mention"
   AIDSTALK11 = "HE-7 HIV Topics Covered in Discussion with Doctor/Provider-11th mention"
   RETROVIR = "HE-8 T/F: Treatment to Prevent Perinatal HIV Transmission"
   PREGHIV = "HE-9 R Tested for HIV During Last Completed Pregnancy"
   EVERVACC = "HF-1 Ever received HPV vaccine?"
   HPVSHOT1 = "HF-2 Age when R received first HPV vaccine shot"
   HPVSEX1 = "HF-2b Which came first - 1st intercourse or 1st HPV vaccine shot"
   VACCPROB = "HF-3 How Likely R Will Receive HPV Vaccine in Next 12 Mos"
   BLDPRESS = "HG-1 R had blood pressure checked in past year"
   HIGHBP = "HG-2 R was told she had high blood pressure in the past year"
   BPMEDS = "HG-3 R currently taking medicine for high blood pressure"
   NUTRINFO_1 = "HG-4 Sources of nutrition information or advice consulted in past 12 months - 1st mention"
   NUTRINFO_2 = "HG-4 Sources of nutrition information or advice consulted in past 12 months - 2nd mention"
   NUTRINFO_3 = "HG-4 Sources of nutrition information or advice consulted in past 12 months - 3rd mention"
   NUTRINFO_4 = "HG-4 Sources of nutrition information or advice consulted in past 12 months - 4th mention"
   NUTRINFO_5 = "HG-4 Sources of nutrition information or advice consulted in past 12 months - 5th mention"
   USUALCAR = "IA-0a Is there place R usually goes when sick"
   USLPLACE = "IA-0b Place R usually goes when sick"
   USL12MOS = "IA-0c R has gone to her usual source of health care in last 12 months"
   CURRCOV = "IA-1 Currently covered by health insurance"
   COVERHOW01 = "IA-2 Type of health care coverage last 12 months - 1st mention"
   COVERHOW02 = "IA-2 Type of health care coverage last 12 months - 2nd mention"
   COVERHOW03 = "IA-2 Type of health care coverage last 12 months - 3rd mention"
   PARINSUR = "IA-3 Covered by parents' plan"
   INS_EXCH = "IA-4 Obtained health insurance through Healthcare.gov or Marketplace"
   INS_PREM = "IA-5 Paid a premium for health plan"
   COVER12 = "IA-6 Whether R lacked health care coverage any time in last 12 months"
   NUMNOCOV = "IA-7 How many of the last 12 months was R without coverage"
   SAMEADD = "IB-1 R living at curr address on April 1, 2010"
   BRNOUT = "IB-8 Whether R was born outside of US"
   RELRAISD = "IC-1/IC-2 Religion in which R was raised"
   ATTND14 = "IC-4 How often R attended religious services at age 14"
   RELCURR = "IC-5/IC-6 Religion R is now"
   RELTRAD = "Current religious affiliation by Protestant categories"
   FUNDAM1 = "IC-8 Fundamental affiliation, if any - 1st mention"
   FUNDAM2 = "IC-8 Fundamental affiliation, if any - 2nd mention"
   FUNDAM3 = "IC-8 Fundamental affiliation, if any - 3rd mention"
   FUNDAM4 = "IC-8 Fundamental affiliation, if any - 4th mention"
   RELDLIFE = "IC-9 How important is religion in R's daily life"
   ATTNDNOW = "IC-10 How often R currently attends religious services"
   MILSVC = "ID-1 R ever on active duty in Armed Forces for 6+ months"
   WRK12MOS = "ID-4 How many of the last 12 months did R work"
   FPT12MOS = "ID-5 Did R work full-time, part-time or both in last 12 months"
   DOLASTWK1 = "IE-1 What R was doing last week (employment status) - 1st mention"
   DOLASTWK2 = "IE-1 What R was doing last week (employment status) - 2nd mention"
   DOLASTWK3 = "IE-1 What R was doing last week (employment status) - 3rd mention"
   DOLASTWK4 = "IE-1 What R was doing last week (employment status) - 4th mention"
   DOLASTWK5 = "IE-1 What R was doing last week (employment status) - 5th mention"
   DOLASTWK6 = "IE-1 What R was doing last week (employment status) - 6th mention"
   RWRKST = "Whether R is currently employed (working or temporarily on leave from a job)"
   WORKP12 = "Whether R worked in the previous 12 months"
   RPAYJOB = "IE-2 Did R ever work at job for pay on regular basis"
   RNUMJOB = "IE-3 Number of jobs R worked last week or last week R worked"
   RFTPTX = "IE-4 R worked full- or part-time at curr/last/primary job"
   REARNTY = "Whether R ever worked at all"
   SPLSTWK1 = "IF-1 H/P doing what last week (employment status) - 1st mention"
   SPLSTWK2 = "IF-1 H/P doing what last week (employment status) - 2nd mention"
   SPLSTWK3 = "IF-1 H/P doing what last week (employment status) - 3rd mention"
   SPLSTWK4 = "IF-1 H/P doing what last week (employment status) - 4th mention"
   SPLSTWK5 = "IF-1 H/P doing what last week (employment status) - 5th mention"
   SPWRKST = "Whether R's husband/partner is currently employed (working or temporarily on leave from a job)"
   SPPAYJOB = "IF-2 Did H/P ever work at a job for pay on regular basis"
   SPNUMJOB = "IF-3 Number of jobs H/P worked last week or last week worked"
   SPFTPTX = "IF-4 H/P worked full- or part-time at curr/last/primary job"
   SPEARNTY = "Whether R's husband/partner ever worked for pay"
   SAMESEX = "IH-1 Sexual relations between two same-sex adults is all right"
   CHSUPPOR = "IH-2 Okay for unmarried woman to have and raise a child"
   REACTSLF = "IH-3 How R would feel if she got pregnant now"
   CHBOTHER = "IH-4 How bothered would R be if she did not have children"
   ACASILANG = "II-6 Language to be used in ACASI"
   GENHEALT = "JA-4 R's General Health"
   BMICAT = "Body Mass Index (categorical)"
   DRWEIGH = "JA-6a Was R weighed in past year by a medical care provider"
   TELLWGHT = "JA-6b Was R told about her weight status"
   WGHTSCRN = "JA-6c Was R provided with diet or exercise counseling by medical care provider"
   ENGSPEAK = "JA-7 How well does R speak English"
   CASIBIRTH = "JB-1 # of pregnancies ending in live birth in 5 yrs bef intvw"
   CASILOSS = "JB-2 # of pregnancies ending in spont loss in 5 yrs bef intvw"
   CASIABOR = "JB-3 # of pregnancies ending in abortion in 5 yrs bef intvw"
   CASIADOP = "JB-4 R ever placed a child she gave birth to for adoption"
   NOBEDYR = "JC-0a R had to stay overnight in a shelter, car or someplace outdoors"
   STAYREL = "JC-0b R had to stay overnight with a friend or relative"
   EVSUSPEN = "JC-0c Ever been suspended or expelled from school?"
   GRADSUSP = "JC-0d Grade when suspended or expelled from school"
   SMK100 = "JC-1 Has R Smoked at Least 100 Cigarettes in Life"
   AGESMK = "JC-2 Age That R First Began Smoking Regularly"
   SMOKE12 = "JC-3 Last 12 months: How Often R Smoked Cigarettes"
   SMKSTOP = "JC-3a Was R provided with counseling to stop smoking"
   DRINK12 = "JC-4 Last 12 months: How Often Drank Alcoholic Beverages"
   UNIT30D = "JC-4a-U Drinking in past 30 days - unit of response"
   DRINK30D = "JC-4a-N Drinking in past 30 days - amount"
   DRINKDAY = "JC-4b Number of drinks R had on average on days R drank"
   BINGE30 = "JC-4c Number of times R binge-drank in past 30 days"
   DRNKMOST = "JC-4d Largest number of drinks R had on any occasion in past 30 days"
   BINGE12 = "JC-5 Last 12 months: How Often Binge Drank (4+ in Couple of Hours)"
   POT12 = "JC-6 Last 12 months: How Often Smoked Marijuana"
   COC12 = "JC-7 Last 12 months: How Often Used Cocaine"
   CRACK12 = "JC-8 Last 12 months: How Often Used Crack"
   CRYSTMTH12 = "JC-8a Last 12 months: How Often Used Crystal/Meth"
   INJECT12 = "JC-9 Last 12 months: How Often Injected Non-Prescription Drugs"
   VAGSEX = "JD-1 Ever Had Vaginal Intercourse with a Male"
   AGEVAGR = "JD-2 R's Age at First Vaginal Intercourse with a Male"
   CONDVAG = "JD-4 R Used Condom at Last Vaginal Intercourse with a Male"
   WHYCONDL = "JD-5 Reason R Used Condom at Last Vaginal Intercourse w/a Male"
   GETORALM = "JD-6 R has ever received oral sex from a male"
   GIVORALM = "JD-7 R has ever performed oral sex on a male"
   CONDFELL = "JD-8 R Used Condom at Last Oral Sex with a Male"
   ANYORAL = "Whether R has ever had oral sex with a male (computed in FC J-8b)"
   TIMING = "JD-8b First oral sex before, after, or during first vag  sex?"
   ANALSEX = "JD-9 R Ever Had Anal Sex with a Male"
   CONDANAL = "JD-10 R Used Condom at Last Anal Sex with a Male"
   OPPSEXANY = "Ever Had Vaginal, Oral, or Anal Sex with a Male (computed in FC J-9)"
   OPPSEXGEN = "Ever Had Male-Genital-Involving Sex with a Male (computed in FC J-9)"
   CONDSEXL = "JD-11 R Used Condom at Last Sex (any type) with a Male"
   OPPLIFENUM = "Number of opposite-sex partners in lifetime for all types of sex (computed in FC J-14d)"
   OPPYEARNUM = "Number of opposite-sex partners in last 12 months for all types of sex. (computed in FC J-14f)"
   VAGNUM12 = "JF-2YRa # male partners past 12 months had vaginal intercourse with"
   ORALNUM12 = "JF-2YRb # male partners past 12 months had oral sex with"
   ANALNUM12 = "JF-2YRc # male partners past 12 months had anal sex with"
   BISEXPRT = "JF-3 Last 12 months: R Had Sex w/Male Who Ever Had Sex w/Males"
   NONMONOG = "JF-4 Last 12 months: R Had Sex w/Male Who Had Other Partners"
   NNONMONOG1 = "JF-5a Number of male partners in last year who were having sex with others"
   NNONMONOG2 = "JF-5b Number of other partners R's 1 partner in last year had at same time"
   NNONMONOG3 = "JF-5c Number of other partners R's most recent partner in last year had at same time"
   MALSHT12 = "JF-6 Last 12 months: R had sex w/male intravenous drug user"
   PROSTFRQ = "JF-7 Last 12 months: R took money or drugs from male for sex"
   JOHNFREQ = "JF-8 Last 12 months: R gave money or drugs to male for sex"
   HIVMAL12 = "JF-9 Last 12 months: R had sex with an HIV-positive male"
   GIVORALF = "JG-1a R performed oral sex on another female?"
   GETORALF = "JG-1b Another female ever performed oral sex on R?"
   FEMSEX = "JG-1c R ever had sexual experience with a female"
   SAMESEXANY = "Ever Had Sexual Contact with a Female (computed in FC J-19)"
   SAMLIFENUM = "Number of same-sex partners in lifetime (computed in FC J-19d)"
   SAMYEARNUM = "Number of same-sex partners in last 12 months (computed in FC J-19f)"
   SAMESEX1 = "JG-4 R's age at first sexual experience with same-sex (female) partner"
   FSAMEREL = "JG-4a R s relationship with 1st female partner at time of 1st sex with her"
   MFLASTP = "JH-1 Was R's Last Sex Partner Male or Female"
   DATEAPP = "JH-1a Had sex with anyone they first met on the internet in past 12 months"
   ATTRACT = "JH-2 R's Sexual Attraction to Males vs Females"
   ORIENT_A = "JH-3 R s Sexual Orientation random 1/2 sample"
   ORIENT_B = "JH-3b R s Sexual Orientation random 1/2 sample"
   CONFCONC = "JH-3a Not go for sexual or reproductive health care because your parents might find out"
   TIMALON = "JH-3b Dr ever spend any time alone with you without a parent"
   RISKCHEK1 = "JH-3c Dr ever asked R about sexual orientation or sex of her partners"
   RISKCHEK2 = "JH-3d Dr ever asked R about her number of sexual partners"
   RISKCHEK3 = "JH-3e Dr ever asked R about her use of condoms"
   RISKCHEK4 = "JH-3f Dr ever asked R about the types of sex she has"
   CHLAMTST = "JH-4 Last 12 months: R tested for chlamydia"
   STDOTHR12 = "JH-4b STD testing (other than chlamydia) in last 12 months"
   STDTRT12 = "JH-5 Last 12 months: R treated for any STD"
   GON = "JH-6 Last 12 months: R told she had gonorrhea"
   CHLAM = "JH-7 Last 12 months: R told she had chlamydia"
   HERPES = "JH-8 R ever (in life) told she had genital herpes"
   GENWARTS = "JH-9 R ever (in life) told she had genital warts"
   SYPHILIS = "JH-10 R ever (in life) told she had syphilis"
   EVRINJECT = "JH-11 Ever injected non-prescription drugs in lifetime"
   EVRSHARE = "JH-12 Ever (In Life) Shared an IV Needle"
   EARNTYPE = "JI-0a Report total earnings before taxes by week/month/year"
   EARN = "JI-0b Total earnings reported by category"
   EARNDK1 = "JI-0c If DK/RF total earnings, was it $20,000 or more"
   EARNDK2 = "JI-0d If more than $20,000, was it $50,000 or more"
   EARNDK3 = "JI-0e If more than $50,000, was it $75,000 or more"
   EARNDK4 = "JI-0f If more than $75,000, was it $100,000 or more"
   TOINCWMY = "JI-2 Prefer to report total income per week/month/year"
   TOTINC = "JI-3 Total combined family income reported by category in prior calendar year"
   FMINCDK1 = "JI-3a If DK/RF combined family income, was it less than $50,000 or $50,000 or more?"
   FMINCDK2 = "JI-3b If income less than $50,000, was it less than $35,000?"
   FMINCDK3 = "JI-3c If income less than $35,000, was it less than poverty threshold?"
   FMINCDK4 = "JI-3d If income more than $50,000, was it $75,000 or more?"
   FMINCDK5 = "JI-3e If income more than $75,000, was it $100,000 or more?"
   PUBASST = "JI-4 Received CASH assistance from state/county welfare program in prior calendar year"
   PUBASTYP1 = "JI-5 Type of CASH assistance program in prior calendar year-1st mention"
   FOODSTMP = "JI-6 In prior calendar year, received food stamps"
   WIC = "JI-7 In prior calendar year, received WIC"
   HLPTRANS = "JI-8a In prior calendar year, received transportation assistance"
   HLPCHLDC = "JI-8b In prior calendar year, received child care services or assistance"
   HLPJOB = "JI-8c In prior calendar year, received job training/search help from social svcs"
   FREEFOOD = "JI-9 In last 12 months, received free or reduced-cost food because couldn't afford to buy food"
   HUNGRY = "JI-10 In last 12 months, a family member was hungry because could not afford more food"
   MED_COST = "JI-11 In last 12 months, a family member didn't see doctor because of cost"
   AGER = "R's age at interview (RECODE)"
   FMARITAL = "Formal (legal) marital status (RECODE)"
   RMARITAL = "Informal marital status (RECODE)"
   EDUCAT = "Education (number of years of schooling) (RECODE)"
   HIEDUC = "Highest completed year of school or highest degree received (RECODE)"
   HISPANIC = "Hispanic origin of respondent (RECODE)"
   RACE = "Race of respondent (RECODE)"
   HISPRACE2 = "Race & Hispanic origin of respondent - 1997 OMB standards (RECODE)"
   NUMKDHH = "Number of bio/adopt/related/legal children under age 18 in household (RECODE)"
   NUMFMHH = "Number of family members in household (RECODE)"
   HHFAMTYP = "Type of household/family structure (RECODE)"
   HHPARTYP = "Type of parental situation in household (RECODE)"
   NCHILDHH = "Number of R's bio or non-bio children (18 or younger) living in household (RECODE)"
   HHKIDTYP = "Whether R has children (18 or younger), and whether bio or non-bio, living in the household (RECODE)"
   CSPBBHH = "Number of R's biological children (18 or younger) with current spouse/cohabiting partner who live in household (RECODE)"
   CSPBSHH = "Number of R's biological children (18 or younger) in household who are the not the biological children of her current husband/cohabiting partner (RECODE)"
   CSPOKDHH = "Number of all other children (18 or younger) living in household with R and current spouse/cohabiting partner (RECODE)"
   INTCTFAM = "Intact status of childhood family (RECODE)"
   PARAGE14 = "Parental living situation at age 14 (RECODE)"
   EDUCMOM = "Mother's (or mother-figure's) education (RECODE)"
   AGEMOMB1 = "Age of mother (or mother-figure) at first birth (RECODE)"
   AGER_I = "AGER Imputation Flag"
   FMARITAL_I = "FMARITAL Imputation Flag"
   RMARITAL_I = "RMARITAL Imputation Flag"
   EDUCAT_I = "EDUCAT Imputation Flag"
   HIEDUC_I = "HIEDUC Imputation Flag"
   HISPANIC_I = "HISPANIC Imputation Flag"
   RACE_I = "RACE Imputation Flag"
   HISPRACE2_I = "HISPRACE2 Imputation Flag"
   NUMKDHH_I = "NUMKDHH Imputation Flag"
   NUMFMHH_I = "NUMFMHH Imputation Flag"
   HHFAMTYP_I = "HHFAMTYP Imputation Flag"
   HHPARTYP_I = "HHPARTYP Imputation Flag"
   NCHILDHH_I = "NCHILDHH Imputation Flag"
   HHKIDTYP_I = "HHKIDTYP Imputation Flag"
   CSPBBHH_I = "CSPBBHH Imputation Flag"
   CSPBSHH_I = "CSPBSHH Imputation Flag"
   CSPOKDHH_I = "CSPOKHH Imputation Flag"
   INTCTFAM_I = "INTCTFAM Imputation Flag"
   PARAGE14_I = "PARAGE14 Imputation Flag"
   EDUCMOM_I = "EDUCMOM Imputation Flag"
   AGEMOMB1_I = "AGEMOMB1 Imputation Flag"
   RCURPREG = "Whether R is currently pregnant (RECODE)"
   PREGNUM = "CAPI-based total number of pregnancies (RECODE)"
   COMPREG = "CAPI-based total number of completed pregnancies (RECODE)"
   LOSSNUM = "CAPI-based total number of spontaneous pregnancy losses (RECODE)"
   ABORTION = "CAPI-based total number of induced abortions (RECODE)"
   LBPREGS = "CAPI-based total number of pregs ending in live birth (RECODE)"
   PARITY = "CAPI-based total number of live births (accounting for mult birth) (RECODE)"
   BIRTHS5 = "Number of live births in last 5 years (RECODE)"
   BIRTH1 = "Whether R had a live birth in the last 12 months (computed in post-processing)"
   OUTCOM01 = "Outcome of pregnancy  - 1st  (RECODE)"
   OUTCOM02 = "Outcome of pregnancy  - 2nd (RECODE)"
   OUTCOM03 = "Outcome of pregnancy  - 3rd (RECODE)"
   OUTCOM04 = "Outcome of pregnancy  - 4th (RECODE)"
   OUTCOM05 = "Outcome of pregnancy  - 5th (RECODE)"
   OUTCOM06 = "Outcome of pregnancy  - 6th (RECODE)"
   OUTCOM07 = "Outcome of pregnancy  - 7th (RECODE)"
   OUTCOM08 = "Outcome of pregnancy  - 8th (RECODE)"
   OUTCOM09 = "Outcome of pregnancy  - 9th (RECODE)"
   OUTCOM10 = "Outcome of pregnancy  - 10th (RECODE)"
   OUTCOM11 = "Outcome of pregnancy  - 11th (RECODE)"
   OUTCOM12 = "Outcome of pregnancy  - 12th (RECODE)"
   OUTCOM13 = "Outcome of pregnancy  - 13th (RECODE)"
   OUTCOM14 = "Outcome of pregnancy  - 14th (RECODE)"
   DATEND01 = "Year when pregnancy ended - 1st (RECODE)"
   DATEND02 = "Year when pregnancy ended - 2nd (RECODE)"
   DATEND03 = "Year when pregnancy ended - 3rd (RECODE)"
   DATEND04 = "Year when pregnancy ended - 4th (RECODE)"
   DATEND05 = "Year when pregnancy ended - 5th (RECODE)"
   DATEND06 = "Year when pregnancy ended - 6th (RECODE)"
   DATEND07 = "Year when pregnancy ended - 7th (RECODE)"
   DATEND08 = "Year when pregnancy ended - 8th (RECODE)"
   DATEND09 = "Year when pregnancy ended - 9th (RECODE)"
   DATEND10 = "Year when pregnancy ended - 10th (RECODE)"
   DATEND11 = "Year when pregnancy ended - 11th (RECODE)"
   DATEND12 = "Year when pregnancy ended - 12th (RECODE)"
   DATEND13 = "Year when pregnancy ended - 13th (RECODE)"
   DATEND14 = "Year when pregnancy ended - 14th (RECODE)"
   AGEPRG01 = "Age when pregnancy ended - 1st (RECODE)"
   AGEPRG02 = "Age when pregnancy ended - 2nd (RECODE)"
   AGEPRG03 = "Age when pregnancy ended - 3rd (RECODE)"
   AGEPRG04 = "Age when pregnancy ended - 4th (RECODE)"
   AGEPRG05 = "Age when pregnancy ended - 5th (RECODE)"
   AGEPRG06 = "Age when pregnancy ended - 6th (RECODE)"
   AGEPRG07 = "Age when pregnancy ended - 7th (RECODE)"
   AGEPRG08 = "Age when pregnancy ended - 8th (RECODE)"
   AGEPRG09 = "Age when pregnancy ended - 9th (RECODE)"
   AGEPRG10 = "Age when pregnancy ended - 10th (RECODE)"
   AGEPRG11 = "Age when pregnancy ended - 11th (RECODE)"
   AGEPRG12 = "Age when pregnancy ended - 12th (RECODE)"
   AGEPRG13 = "Age when pregnancy ended - 13th (RECODE)"
   AGEPRG14 = "Age when pregnancy ended - 14th (RECODE)"
   DATCON01 = "Year when pregnancy began - 1st (RECODE)"
   DATCON02 = "Year when pregnancy began - 2nd (RECODE)"
   DATCON03 = "Year when pregnancy began - 3rd (RECODE)"
   DATCON04 = "Year when pregnancy began - 4th (RECODE)"
   DATCON05 = "Year when pregnancy began - 5th (RECODE)"
   DATCON06 = "Year when pregnancy began - 6th (RECODE)"
   DATCON07 = "Year when pregnancy began - 7th (RECODE)"
   DATCON08 = "Year when pregnancy began - 8th (RECODE)"
   DATCON09 = "Year when pregnancy began - 9th (RECODE)"
   DATCON10 = "Year when pregnancy began - 10th (RECODE)"
   DATCON11 = "Year when pregnancy began - 11th (RECODE)"
   DATCON12 = "Year when pregnancy began - 12th (RECODE)"
   DATCON13 = "Year when pregnancy began - 13th (RECODE)"
   DATCON14 = "Year when pregnancy began - 14th (RECODE)"
   AGECON01 = "Age when pregnancy began - 1st (RECODE)"
   AGECON02 = "Age when pregnancy began - 2nd (RECODE)"
   AGECON03 = "Age when pregnancy began - 3rd (RECODE)"
   AGECON04 = "Age when pregnancy began - 4th (RECODE)"
   AGECON05 = "Age when pregnancy began - 5th (RECODE)"
   AGECON06 = "Age when pregnancy began - 6th (RECODE)"
   AGECON07 = "Age when pregnancy began - 7th (RECODE)"
   AGECON08 = "Age when pregnancy began - 8th (RECODE)"
   AGECON09 = "Age when pregnancy began - 9th (RECODE)"
   AGECON10 = "Age when pregnancy began - 10th (RECODE)"
   AGECON11 = "Age when pregnancy began - 11th (RECODE)"
   AGECON12 = "Age when pregnancy began - 12th (RECODE)"
   AGECON13 = "Age when pregnancy began - 13th (RECODE)"
   AGECON14 = "Age when pregnancy began - 14th (RECODE)"
   MAROUT01 = "Formal marital status when pregnancy ended - 1st (RECODE)"
   MAROUT02 = "Formal marital status when pregnancy ended - 2nd (RECODE)"
   MAROUT03 = "Formal marital status when pregnancy ended - 3rd (RECODE)"
   MAROUT04 = "Formal marital status when pregnancy ended - 4th (RECODE)"
   MAROUT05 = "Formal marital status when pregnancy ended - 5th (RECODE)"
   MAROUT06 = "Formal marital status when pregnancy ended - 6th (RECODE)"
   MAROUT07 = "Formal marital status when pregnancy ended - 7th (RECODE)"
   MAROUT08 = "Formal marital status when pregnancy ended - 8th (RECODE)"
   MAROUT09 = "Formal marital status when pregnancy ended - 9th (RECODE)"
   MAROUT10 = "Formal marital status when pregnancy ended - 10th (RECODE)"
   MAROUT11 = "Formal marital status when pregnancy ended - 11th (RECODE)"
   MAROUT12 = "Formal marital status when pregnancy ended - 12th (RECODE)"
   MAROUT13 = "Formal marital status when pregnancy ended - 13th (RECODE)"
   MAROUT14 = "Formal marital status when pregnancy ended - 14th (RECODE)"
   RMAROUT01 = "Informal marital status when pregnancy ended - 1st (RECODE)"
   RMAROUT02 = "Informal marital status when pregnancy ended - 2nd (RECODE)"
   RMAROUT03 = "Informal marital status when pregnancy ended - 3rd (RECODE)"
   RMAROUT04 = "Informal marital status when pregnancy ended - 4th (RECODE)"
   RMAROUT05 = "Informal marital status when pregnancy ended - 5th (RECODE)"
   RMAROUT06 = "Informal marital status when pregnancy ended - 6th (RECODE)"
   RMAROUT07 = "Informal marital status when pregnancy ended - 7th (RECODE)"
   RMAROUT08 = "Informal marital status when pregnancy ended - 8th (RECODE)"
   RMAROUT09 = "Informal marital status when pregnancy ended - 9th (RECODE)"
   RMAROUT10 = "Informal marital status when pregnancy ended - 10th (RECODE)"
   RMAROUT11 = "Informal marital status when pregnancy ended - 11th (RECODE)"
   RMAROUT12 = "Informal marital status when pregnancy ended - 12th (RECODE)"
   RMAROUT13 = "Informal marital status when pregnancy ended - 13th (RECODE)"
   RMAROUT14 = "Informal marital status when pregnancy ended - 14th (RECODE)"
   MARCON01 = "Formal marital status when pregnancy began - 1st (RECODE)"
   MARCON02 = "Formal marital status when pregnancy began - 2nd (RECODE)"
   MARCON03 = "Formal marital status when pregnancy began - 3rd (RECODE)"
   MARCON04 = "Formal marital status when pregnancy began - 4th (RECODE)"
   MARCON05 = "Formal marital status when pregnancy began - 5th (RECODE)"
   MARCON06 = "Formal marital status when pregnancy began - 6th (RECODE)"
   MARCON07 = "Formal marital status when pregnancy began - 7th (RECODE)"
   MARCON08 = "Formal marital status when pregnancy began - 8th (RECODE)"
   MARCON09 = "Formal marital status when pregnancy began - 9th (RECODE)"
   MARCON10 = "Formal marital status when pregnancy began - 10th (RECODE)"
   MARCON11 = "Formal marital status when pregnancy began - 11th (RECODE)"
   MARCON12 = "Formal marital status when pregnancy began - 12th (RECODE)"
   MARCON13 = "Formal marital status when pregnancy began - 13th (RECODE)"
   MARCON14 = "Formal marital status when pregnancy began - 14th (RECODE)"
   RMARCON01 = "Informal marital status when pregnancy began - 1st (RECODE)"
   RMARCON02 = "Informal marital status when pregnancy began - 2nd (RECODE)"
   RMARCON03 = "Informal marital status when pregnancy began - 3rd (RECODE)"
   RMARCON04 = "Informal marital status when pregnancy began - 4th (RECODE)"
   RMARCON05 = "Informal marital status when pregnancy began - 5th (RECODE)"
   RMARCON06 = "Informal marital status when pregnancy began - 6th (RECODE)"
   RMARCON07 = "Informal marital status when pregnancy began - 7th (RECODE)"
   RMARCON08 = "Informal marital status when pregnancy began - 8th (RECODE)"
   RMARCON09 = "Informal marital status when pregnancy began - 9th (RECODE)"
   RMARCON10 = "Informal marital status when pregnancy began - 10th (RECODE)"
   RMARCON11 = "Informal marital status when pregnancy began - 11th (RECODE)"
   RMARCON12 = "Informal marital status when pregnancy began - 12th (RECODE)"
   RMARCON13 = "Informal marital status when pregnancy began - 13th (RECODE)"
   RMARCON14 = "Informal marital status when pregnancy began - 14th (RECODE)"
   CEBOW = "Number of children born out of wedlock (RECODE)"
   CEBOWC = "Number of children born in cohabiting unions (RECODE)"
   DATBABY1 = "Year of 1st live birth (RECODE)"
   AGEBABY1 = "Age at 1st live birth (RECODE)"
   LASTPREG = "How recent was R's last pregnancy (computed in post-processing)"
   PREG_INT1 = "Inter-pregnancy interval  (in months) between 1st live birth and 2nd pregnancy conception, regardless of outcome (computed in post-processing)"
   PREG_INT2 = "Inter-pregnancy interval  (in months) between 2nd live birth and 3rd pregnancy conception, regardless of outcome (computed in post-processing)"
   PREG_INT3 = "Inter-pregnancy interval  (in months) between most recent pregnancy conception and prior live birth (computed in post-processing)"
   PREG_INT4 = "Inter-pregnancy interval  (in months) between most recent pregnancy conception and prior pregnancy end date, regardless of outcome (computed in post-processing)"
   RCURPREG_I = "RCURPREG Imputation Flag"
   PREGNUM_I = "PREGNUM Imputation Flag"
   COMPREG_I = "COMPREG Imputation Flag"
   LOSSNUM_I = "LOSSNUM Imputation Flag"
   ABORTION_I = "ABORTION Imputation Flag"
   LBPREGS_I = "LBPREGS Imputation Flag"
   PARITY_I = "PARITY Imputation Flag"
   BIRTHS5_I = "BIRTHS5 Imputation Flag"
   OUTCOM01_I = "OUTCOM01 Imputation Flag"
   OUTCOM02_I = "OUTCOM02 Imputation Flag"
   OUTCOM03_I = "OUTCOM03 Imputation Flag"
   OUTCOM04_I = "OUTCOM04 Imputation Flag"
   OUTCOM05_I = "OUTCOM05 Imputation Flag"
   OUTCOM06_I = "OUTCOM06 Imputation Flag"
   OUTCOM07_I = "OUTCOM07 Imputation Flag"
   OUTCOM08_I = "OUTCOM08 Imputation Flag"
   OUTCOM09_I = "OUTCOM09 Imputation Flag"
   OUTCOM10_I = "OUTCOM10 Imputation Flag"
   OUTCOM11_I = "OUTCOM11 Imputation Flag"
   OUTCOM12_I = "OUTCOM12 Imputation Flag"
   OUTCOM13_I = "OUTCOM13 Imputation Flag"
   OUTCOM14_I = "OUTCOM14 Imputation Flag"
   DATEND01_I = "DATEND01 Imputation Flag"
   DATEND02_I = "DATEND02 Imputation Flag"
   DATEND03_I = "DATEND03 Imputation Flag"
   DATEND04_I = "DATEND04 Imputation Flag"
   DATEND05_I = "DATEND05 Imputation Flag"
   DATEND06_I = "DATEND06 Imputation Flag"
   DATEND07_I = "DATEND07 Imputation Flag"
   DATEND08_I = "DATEND08 Imputation Flag"
   DATEND09_I = "DATEND09 Imputation Flag"
   DATEND10_I = "DATEND10 Imputation Flag"
   DATEND11_I = "DATEND11 Imputation Flag"
   DATEND12_I = "DATEND12 Imputation Flag"
   DATEND13_I = "DATEND13 Imputation Flag"
   DATEND14_I = "DATEND14 Imputation Flag"
   AGEPRG01_I = "AGEPRG01 Imputation Flag"
   AGEPRG02_I = "AGEPRG02 Imputation Flag"
   AGEPRG03_I = "AGEPRG03 Imputation Flag"
   AGEPRG04_I = "AGEPRG04 Imputation Flag"
   AGEPRG05_I = "AGEPRG05 Imputation Flag"
   AGEPRG06_I = "AGEPRG06 Imputation Flag"
   AGEPRG07_I = "AGEPRG07 Imputation Flag"
   AGEPRG08_I = "AGEPRG08 Imputation Flag"
   AGEPRG09_I = "AGEPRG09 Imputation Flag"
   AGEPRG10_I = "AGEPRG10 Imputation Flag"
   AGEPRG11_I = "AGEPRG11 Imputation Flag"
   AGEPRG12_I = "AGEPRG12 Imputation Flag"
   AGEPRG13_I = "AGEPRG13 Imputation Flag"
   AGEPRG14_I = "AGEPRG14 Imputation Flag"
   DATCON01_I = "DATCON01 Imputation Flag"
   DATCON02_I = "DATCON02 Imputation Flag"
   DATCON03_I = "DATCON03 Imputation Flag"
   DATCON04_I = "DATCON04 Imputation Flag"
   DATCON05_I = "DATCON05 Imputation Flag"
   DATCON06_I = "DATCON06 Imputation Flag"
   DATCON07_I = "DATCON07 Imputation Flag"
   DATCON08_I = "DATCON08 Imputation Flag"
   DATCON09_I = "DATCON09 Imputation Flag"
   DATCON10_I = "DATCON10 Imputation Flag"
   DATCON11_I = "DATCON11 Imputation Flag"
   DATCON12_I = "DATCON12 Imputation Flag"
   DATCON13_I = "DATCON13 Imputation Flag"
   DATCON14_I = "DATCON14 Imputation Flag"
   AGECON01_I = "AGECON01 Imputation Flag"
   AGECON02_I = "AGECON02 Imputation Flag"
   AGECON03_I = "AGECON03 Imputation Flag"
   AGECON04_I = "AGECON04 Imputation Flag"
   AGECON05_I = "AGECON05 Imputation Flag"
   AGECON06_I = "AGECON06 Imputation Flag"
   AGECON07_I = "AGECON07 Imputation Flag"
   AGECON08_I = "AGECON08 Imputation Flag"
   AGECON09_I = "AGECON09 Imputation Flag"
   AGECON10_I = "AGECON10 Imputation Flag"
   AGECON11_I = "AGECON11 Imputation Flag"
   AGECON12_I = "AGECON12 Imputation Flag"
   AGECON13_I = "AGECON13 Imputation Flag"
   AGECON14_I = "AGECON14 Imputation Flag"
   MAROUT01_I = "MAROUT01 Imputation Flag"
   MAROUT02_I = "MAROUT02 Imputation Flag"
   MAROUT03_I = "MAROUT03 Imputation Flag"
   MAROUT04_I = "MAROUT04 Imputation Flag"
   MAROUT05_I = "MAROUT05 Imputation Flag"
   MAROUT06_I = "MAROUT06 Imputation Flag"
   MAROUT07_I = "MAROUT07 Imputation Flag"
   MAROUT08_I = "MAROUT08 Imputation Flag"
   MAROUT09_I = "MAROUT09 Imputation Flag"
   MAROUT10_I = "MAROUT10 Imputation Flag"
   MAROUT11_I = "MAROUT11 Imputation Flag"
   MAROUT12_I = "MAROUT12 Imputation Flag"
   MAROUT13_I = "MAROUT13 Imputation Flag"
   MAROUT14_I = "MAROUT14 Imputation Flag"
   RMAROUT01_I = "RMAROUT01 Imputation Flag"
   RMAROUT02_I = "RMAROUT02 Imputation Flag"
   RMAROUT03_I = "RMAROUT03 Imputation Flag"
   RMAROUT04_I = "RMAROUT04 Imputation Flag"
   RMAROUT05_I = "RMAROUT05 Imputation Flag"
   RMAROUT06_I = "RMAROUT06 Imputation Flag"
   RMAROUT07_I = "RMAROUT07 Imputation Flag"
   RMAROUT08_I = "RMAROUT08 Imputation Flag"
   RMAROUT09_I = "RMAROUT09 Imputation Flag"
   RMAROUT10_I = "RMAROUT10 Imputation Flag"
   RMAROUT11_I = "RMAROUT11 Imputation Flag"
   RMAROUT12_I = "RMAROUT12 Imputation Flag"
   RMAROUT13_I = "RMAROUT13 Imputation Flag"
   RMAROUT14_I = "RMAROUT14 Imputation Flag"
   MARCON01_I = "MARCON01 Imputation Flag"
   MARCON02_I = "MARCON02 Imputation Flag"
   MARCON03_I = "MARCON03 Imputation Flag"
   MARCON04_I = "MARCON04 Imputation Flag"
   MARCON05_I = "MARCON05 Imputation Flag"
   MARCON06_I = "MARCON06 Imputation Flag"
   MARCON07_I = "MARCON07 Imputation Flag"
   MARCON08_I = "MARCON08 Imputation Flag"
   MARCON09_I = "MARCON09 Imputation Flag"
   MARCON10_I = "MARCON10 Imputation Flag"
   MARCON11_I = "MARCON11 Imputation Flag"
   MARCON12_I = "MARCON12 Imputation Flag"
   MARCON13_I = "MARCON13 Imputation Flag"
   MARCON14_I = "MARCON14 Imputation Flag"
   RMARCON01_I = "RMARCON01 Imputation Flag"
   RMARCON02_I = "RMARCON02 Imputation Flag"
   RMARCON03_I = "RMARCON03 Imputation Flag"
   RMARCON04_I = "RMARCON04 Imputation Flag"
   RMARCON05_I = "RMARCON05 Imputation Flag"
   RMARCON06_I = "RMARCON06 Imputation Flag"
   RMARCON07_I = "RMARCON07 Imputation Flag"
   RMARCON08_I = "RMARCON08 Imputation Flag"
   RMARCON09_I = "RMARCON09 Imputation Flag"
   RMARCON10_I = "RMARCON10 Imputation Flag"
   RMARCON11_I = "RMARCON11 Imputation Flag"
   RMARCON12_I = "RMARCON12 Imputation Flag"
   RMARCON13_I = "RMARCON13 Imputation Flag"
   RMARCON14_I = "RMARCON14 Imputation Flag"
   CEBOW_I = "CEBOW Imputation Flag"
   CEBOWC_I = "CEBOWC Imputation Flag"
   DATBABY1_I = "DATBABY1 Imputation Flag"
   AGEBABY1_I = "AGEBABY1 Imputation Flag"
   FMARNO = "Number of times R has been married (RECODE)"
   CSPBIOKD = "Total number of biological children R has had with current spouse or cohabiting partner (RECODE)"
   MARDAT01 = "Year of R's 1st marriage (RECODE)"
   MARDAT02 = "Year of R's 2nd marriage (RECODE)"
   FMAR2AGE = "R's age at 2nd formal marriage"
   MARDAT03 = "Year of R's 3rd marriage (RECODE)"
   FMAR3AGE = "R's age at 3rd formal marriage"
   MARDAT04 = "Year of R's 4th marriage (RECODE)"
   FMAR4AGE = "R's age at 4th formal marriage"
   MARDIS01 = "Year when R's 1st marriage ended (RECODE)"
   MARDIS02 = "Year when R's 2nd marriage ended (RECODE)"
   MARDIS03 = "Year when R's 3rd marriage ended (RECODE)"
   MARDIS04 = "Year when R's 4th marriage ended (RECODE)"
   MAREND01 = "How R's 1st marriage ended (RECODE)"
   MAREND02 = "How R's 2nd marriage ended (RECODE)"
   MAREND03 = "How R's 3rd marriage ended (RECODE)"
   MAREND04 = "How R's 4th marriage ended (RECODE)"
   FMAR1AGE = "Age at first marriage (RECODE)"
   AGEDISS1 = "Age at dissolution of first marriage (RECODE)"
   AGEDD1 = "Age at divorce or death:  1st marriage (RECODE)"
   MAR1DISS = "Months btw/1st marriage & dissolution (or interview) (RECODE)"
   DD1REMAR = "Months btw/divorce or death (1st marriage) and remarriage (or interview) (RECODE)"
   MAR1BIR1 = "Months btw/1st marriage & 1st birth (or interview) (RECODE)"
   MAR1CON1 = "Months btw/1st marriage & 1st conception (or interview) (RECODE)"
   CON1MAR1 = "Months btw/1st conception & 1st marriage (or interview) (RECODE)"
   B1PREMAR = "Whether R's first birth was premarital (RECODE)"
   COHEVER = "Whether R ever cohabited outside of marriage (RECODE)"
   EVMARCOH = "Whether R ever married or cohabited (RECODE)"
   PMARRNO = "Number of premarital cohabitations (RECODE)"
   NONMARR = "Number of nonmarital cohabitations (RECODE)"
   TIMESCOH = "Total number of cohabitations  (RECODE)"
   COHAB1 = "Year of 1st cohabitation (incl. premarital cohabitation) (RECODE)"
   COHSTAT = "Cohabitation experience relative to 1st marriage (RECODE)"
   COHOUT = "Outcome of 1st cohabitation (RECODE)"
   COH1DUR = "Duration (in months) of R's first cohabitation (RECODE)"
   HADSEX = "Whether R has ever had sexual intercourse with a male (RECODE)"
   SEXONCE = "Whether R has had sex only once (RECODE)"
   SEXEVER = "Whether R has ever had sexual intercourse with a male since menarche (RECODE)"
   VRY1STAG = "Age at first intercourse (even if before menarche) (RECODE)"
   SEX1AGE = "Age at first intercourse since menarche (RECODE)"
   VRY1STSX = "CM date of first intercourse (even if before menarche) (RECODE)"
   DATESEX1 = "CM date of first intercourse since menarche (RECODE)"
   SEXMAR = "Mos btw/1st intercourse (even if before menarche) & 1st marriage (or interview) (RECODE)"
   SEX1FOR = "Mos btw/1st intercourse since menarche & 1st marriage (or interview)  (RECODE)"
   SEXUNION = "Months between 1st intercourse and 1st coresidential union (marriage or cohabitation) (RECODE)"
   SEXOUT = "Outcome of 1st sexual intercourse (RECODE)"
   FPDUR = "Months between 1st and last/most recent intercourse with 1st partner ever (RECODE)"
   PARTS1YR = "Number of opposite-sex partners in last 12 mos (RECODE)"
   LSEXDATE = "CM date of last or most recent sexual intercourse (RECODE)"
   LSEXPREG = "Whether R was pregnant in month of last sexual intercourse in the past 12 months (computed in post-processing)"
   SEX3MO = "Whether R had sex in past 3 months (from sexual partner dates) (RECODE)"
   NUMP3MOS = "Number of male partners in last 3 mos (RECODE)"
   LSEXRAGE = "R's age at last or most recent sexual intercourse (RECODE)"
   PARTDUR1 = "Months between 1st and most recent intercourse with last/most recent partner (RECODE)"
   PARTDUR2 = "Months between 1st and most recent intercourse with 2nd-to-last partner within past 12 mos (RECODE)"
   PARTDUR3 = "Months between 1st and most recent intercourse with 3rd-to-last partner within past 12 mos (RECODE)"
   RELATP1 = "Relationship at time of 1st sex with last/most recent partner (RECODE)"
   RELATP2 = "Relationship at time of 1st sex with 2nd-to-last partner within past 12 mos (RECODE)"
   RELATP3 = "Relationship at time of 1st sex with 3rd-to-last partner within past 12 mos (RECODE)"
   LIFPRTNR = "Number of opposite-sex partners in lifetime  (RECODE)"
   FMARNO_I = "FMARNO Imputation Flag"
   CSPBIOKD_I = "CSPBIOKD Imputation Flag"
   MARDAT01_I = "MARDAT01 Imputation Flag"
   MARDAT02_I = "MARDAT02 Imputation Flag"
   MARDAT03_I = "MARDAT03 Imputation Flag"
   MARDAT04_I = "MARDAT04 Imputation Flag"
   MARDIS01_I = "MARDIS01 Imputation Flag"
   MARDIS02_I = "MARDIS02 Imputation Flag"
   MARDIS03_I = "MARDIS03 Imputation Flag"
   MARDIS04_I = "MARDIS04 Imputation Flag"
   MAREND01_I = "MAREND1 Imputation Flag"
   MAREND02_I = "MAREND2 Imputation Flag"
   MAREND03_I = "MAREND3 Imputation Flag"
   MAREND04_I = "MAREND4 Imputation Flag"
   FMAR1AGE_I = "FMAR1AGE Imputation Flag"
   AGEDISS1_I = "AGEDISS1 Imputation Flag"
   AGEDD1_I = "AGEDD1 Imputation Flag"
   MAR1DISS_I = "MAR1DISS Imputation Flag"
   DD1REMAR_I = "DD1REMAR Imputation Flag"
   MAR1BIR1_I = "MAR1BIR1 Imputation Flag"
   MAR1CON1_I = "MAR1CON1 Imputation Flag"
   CON1MAR1_I = "CON1MAR1 Imputation Flag"
   B1PREMAR_I = "B1PREMAR Imputation Flag"
   COHEVER_I = "COHEVER Imputation Flag"
   EVMARCOH_I = "EVMARCOH Imputation Flag"
   PMARRNO_I = "PMARRNO Imputation Flag"
   NONMARR_I = "NONMARR Imputation Flag"
   TIMESCOH_I = "TIMESCOH Imputation Flag"
   COHAB1_I = "COHAB1 Imputation Flag"
   COHSTAT_I = "COHSTAT Imputation Flag"
   COHOUT_I = "COHOUT Imputation Flag"
   COH1DUR_I = "COH1DUR Imputation Flag"
   HADSEX_I = "HADSEX Imputation Flag"
   SEXEVER_I = "SEXEVER Imputation Flag"
   VRY1STAG_I = "VRY1STAG Imputation Flag"
   SEX1AGE_I = "SEX1AGE Imputation Flag"
   VRY1STSX_I = "VRY1STSX Imputation Flag"
   DATESEX1_I = "DATESEX1 Imputation Flag"
   SEXONCE_I = "SEXONCE Imputation Flag"
   SEXMAR_I = "SEXMAR Imputation Flag"
   SEX1FOR_I = "SEX1FOR Imputation Flag"
   SEXUNION_I = "SEXUNION Imputation Flag"
   SEXOUT_I = "SEXOUT Imputation Flag"
   FPDUR_I = "FPDUR Imputation Flag"
   PARTS1YR_I = "PARTS1YR Imputation Flag"
   LSEXDATE_I = "LSEXDATE Imputation Flag"
   SEXP3MO_I = "SEXP3MO Imputation Flag"
   NUMP3MOS_I = "NUMP3MOS Imputation Flag"
   LSEXRAGE_I = "LSEXRAGE Imputation Flag"
   PARTDUR1_I = "PARTDUR1 Imputation Flag"
   PARTDUR2_I = "PARTDUR2 Imputation Flag"
   PARTDUR3_I = "PARTDUR3 Imputation Flag"
   RELATP1_I = "RELATP1 Imputation Flag"
   RELATP2_I = "RELATP2 Imputation Flag"
   RELATP3_I = "RELATP3 Imputation Flag"
   LIFPRTNR_I = "LIFPRTNR Imputation Flag"
   STRLOPER = "Type of sterilization operation 'in effect' (RECODE)"
   FECUND = "Fecundity status (RECODE)"
   ANYBC36 = "Any method use in 36 months before interview"
   NOSEX36 = "Any months of NONintercourse in 36 months before interview"
   INFERT = "Infertility status (RECODE)"
   ANYBC12 = "Any method use in 12 months before interview"
   ANYMTHD = "Ever used any method for any reason (RECODE)"
   NOSEX12 = "Number of months of nonintercourse in the 12 months prior to interview (RECODE)"
   SEXP3MO = "Whether R had sex in past 3 months (from months of nonintercourse series) (RECODE)"
   CONSTAT1 = "Current contraceptive status (1st priority code) (RECODE)"
   CONSTAT2 = "2nd priority code for current contraceptive status (RECODE)"
   CONSTAT3 = "3rd priority code for current contraceptive status (RECODE)"
   CONSTAT4 = "4th priority code for current contraceptive status (RECODE)"
   PILLR = "Ever used the pill for any reason (RECODE)"
   CONDOMR = "Ever used condom (RECODE)"
   SEX1MTHD1 = "Contraceptive method used at first sex, if any: 1st mentioned (RECODE)"
   SEX1MTHD2 = "Contraceptive method used at first sex, if any: 2nd mentioned (RECODE)"
   SEX1MTHD3 = "Contraceptive method used at first sex, if any: 3rd mentioned (RECODE)"
   SEX1MTHD4 = "Contraceptive method used at first sex, if any: 4th mentioned (RECODE)"
   MTHUSE12 = "Whether used any contraceptive method at last sex in past 12 months (RECODE)"
   METH12M1 = "Contraceptive method used at last sex past 12 mos: 1st mentioned (RECODE)"
   METH12M2 = "Contraceptive method used at last sex past 12 mos: 2nd mentioned (RECODE)"
   METH12M3 = "Contraceptive method used at last sex past 12 mos: 3rd mentioned (RECODE)"
   METH12M4 = "Contraceptive method used at last sex past 12 mos: 4th mentioned (RECODE)"
   MTHUSE3 = "Whether used any contraceptive method at last sex in past 3 mos (RECODE)"
   METH3M1 = "Contraceptive method used last sex past 3 mos: 1st mentioned (RECODE)"
   METH3M2 = "Contraceptive method used last sex past 3 mos: 2nd mentioned (RECODE)"
   METH3M3 = "Contraceptive method used last sex past 3 mos: 3rd mentioned (RECODE)"
   METH3M4 = "Contraceptive method used last sex past 3 mos: 4th mentioned (RECODE)"
   FMETHOD1 = "First method ever used: 1st mentioned (RECODE)"
   FMETHOD2 = "First method ever used: 2nd mentioned (RECODE)"
   FMETHOD3 = "First method ever used: 3rd mentioned (RECODE)"
   FMETHOD4 = "First method ever used: 4th mentioned (RECODE)"
   DATEUSE1 = "Date R used first method for the first time (RECODE)"
   OLDWP01 = "Wantedness of 1st pregnancy--R's partner--cycle 4 version (RECODE)"
   OLDWP02 = "Wantedness of 2nd pregnancy--R's partner--cycle 4 version (RECODE)"
   OLDWP03 = "Wantedness of 3rd pregnancy--R's partner--cycle 4 version (RECODE)"
   OLDWP04 = "Wantedness of 4th pregnancy--R's partner--cycle 4 version (RECODE)"
   OLDWP05 = "Wantedness of 5th pregnancy--R's partner--cycle 4 version (RECODE)"
   OLDWP06 = "Wantedness of 6th pregnancy--R's partner--cycle 4 version (RECODE)"
   OLDWP07 = "Wantedness of 7th pregnancy--R's partner--cycle 4 version (RECODE)"
   OLDWP08 = "Wantedness of 8th pregnancy--R's partner--cycle 4 version (RECODE)"
   OLDWP09 = "Wantedness of 9th pregnancy--R's partner--cycle 4 version (RECODE)"
   OLDWP10 = "Wantedness of 10th pregnancy--R's partner--cycle 4 version (RECODE)"
   OLDWP11 = "Wantedness of 11th pregnancy--R's partner--cycle 4 version (RECODE)"
   OLDWP12 = "Wantedness of 12th pregnancy--R's partner--cycle 4 version (RECODE)"
   OLDWP13 = "Wantedness of 13th pregnancy--R's partner--cycle 4 version (RECODE)"
   OLDWP14 = "Wantedness of 14th pregnancy--R's partner--cycle 4 version (RECODE)"
   OLDWR01 = "Wantedness of 1st pregnancy--Respondent--cycle 4 version (RECODE)"
   OLDWR02 = "Wantedness of 2nd pregnancy--Respondent--cycle 4 version (RECODE)"
   OLDWR03 = "Wantedness of 3rd pregnancy--Respondent--cycle 4 version (RECODE)"
   OLDWR04 = "Wantedness of 4th pregnancy--Respondent--cycle 4 version (RECODE)"
   OLDWR05 = "Wantedness of 5th pregnancy--Respondent--cycle 4 version (RECODE)"
   OLDWR06 = "Wantedness of 6th pregnancy--Respondent--cycle 4 version (RECODE)"
   OLDWR07 = "Wantedness of 7th pregnancy--Respondent--cycle 4 version (RECODE)"
   OLDWR08 = "Wantedness of 8th pregnancy--Respondent--cycle 4 version (RECODE)"
   OLDWR09 = "Wantedness of 9th pregnancy--Respondent--cycle 4 version (RECODE)"
   OLDWR10 = "Wantedness of 10th pregnancy--Respondent--cycle 4 version (RECODE)"
   OLDWR11 = "Wantedness of 11th pregnancy--Respondent--cycle 4 version (RECODE)"
   OLDWR12 = "Wantedness of 12th pregnancy--Respondent--cycle 4 version (RECODE)"
   OLDWR13 = "Wantedness of 13th pregnancy--Respondent--cycle 4 version (RECODE)"
   OLDWR14 = "Wantedness of 14th pregnancy--Respondent--cycle 4 version (RECODE)"
   WANTRP01 = "Wantedness of 1st pregnancy--Respondent (RECODE)"
   WANTRP02 = "Wantedness of 2nd pregnancy--Respondent (RECODE)"
   WANTRP03 = "Wantedness of 3rd pregnancy--Respondent (RECODE)"
   WANTRP04 = "Wantedness of 4th pregnancy--Respondent (RECODE)"
   WANTRP05 = "Wantedness of 5th pregnancy--Respondent (RECODE)"
   WANTRP06 = "Wantedness of 6th pregnancy--Respondent (RECODE)"
   WANTRP07 = "Wantedness of 7th pregnancy--Respondent (RECODE)"
   WANTRP08 = "Wantedness of 8th pregnancy--Respondent (RECODE)"
   WANTRP09 = "Wantedness of 9th pregnancy--Respondent (RECODE)"
   WANTRP10 = "Wantedness of 10th pregnancy--Respondent (RECODE)"
   WANTRP11 = "Wantedness of 11th pregnancy--Respondent (RECODE)"
   WANTRP12 = "Wantedness of 12th pregnancy--Respondent (RECODE)"
   WANTRP13 = "Wantedness of 13th pregnancy--Respondent (RECODE)"
   WANTRP14 = "Wantedness of 14th pregnancy--Respondent (RECODE)"
   WANTP01 = "Wantedness of 1st pregnancy--R's partner (RECODE)"
   WANTP02 = "Wantedness of 2nd pregnancy--R's partner (RECODE)"
   WANTP03 = "Wantedness of 3rd pregnancy--R's partner (RECODE)"
   WANTP04 = "Wantedness of 4th pregnancy--R's partner (RECODE)"
   WANTP05 = "Wantedness of 5th pregnancy--R's partner (RECODE)"
   WANTP06 = "Wantedness of 6th pregnancy--R's partner (RECODE)"
   WANTP07 = "Wantedness of 7th pregnancy--R's partner (RECODE)"
   WANTP08 = "Wantedness of 8th pregnancy--R's partner (RECODE)"
   WANTP09 = "Wantedness of 9th pregnancy--R's partner (RECODE)"
   WANTP10 = "Wantedness of 10th pregnancy--R's partner (RECODE)"
   WANTP11 = "Wantedness of 11th pregnancy--R's partner (RECODE)"
   WANTP12 = "Wantedness of 12th pregnancy--R's partner (RECODE)"
   WANTP13 = "Wantedness of 13th pregnancy--R's partner (RECODE)"
   WANTP14 = "Wantedness of 14th pregnancy--R's partner (RECODE)"
   NWWANTRP01 = "Detailed wantedness of 1st pregnancy--Respondent (RECODE)"
   NWWANTRP02 = "Detailed wantedness of 2nd pregnancy--Respondent (RECODE)"
   NWWANTRP03 = "Detailed wantedness of 3rd pregnancy--Respondent (RECODE)"
   NWWANTRP04 = "Detailed wantedness of 4th pregnancy--Respondent (RECODE)"
   NWWANTRP05 = "Detailed wantedness of 5th pregnancy--Respondent (RECODE)"
   NWWANTRP06 = "Detailed wantedness of 6th pregnancy--Respondent (RECODE)"
   NWWANTRP07 = "Detailed wantedness of 7th pregnancy--Respondent (RECODE)"
   NWWANTRP08 = "Detailed wantedness of 8th pregnancy--Respondent (RECODE)"
   NWWANTRP09 = "Detailed wantedness of 9th pregnancy--Respondent (RECODE)"
   NWWANTRP10 = "Detailed wantedness of 10th pregnancy--Respondent (RECODE)"
   NWWANTRP11 = "Detailed wantedness of 11th pregnancy--Respondent (RECODE)"
   NWWANTRP12 = "Detailed wantedness of 12th pregnancy--Respondent (RECODE)"
   NWWANTRP13 = "Detailed wantedness of 13th pregnancy--Respondent (RECODE)"
   NWWANTRP14 = "Detailed wantedness of 14th pregnancy--Respondent (RECODE)"
   WANTP5 = "Number of wanted pregnancies in the last 5 years (RECODE)"
   STRLOPER_I = "STRLOPER Imputation Flag"
   FECUND_I = "FECUND Imputation Flag"
   INFERT_I = "INFERT Imputation Flag"
   ANYMTHD_I = "ANYMTHD Imputation Flag"
   NOSEX12_I = "NOSEX12 Imputation Flag"
   SEX3MO_I = "SEX3MO Imputation Flag"
   CONSTAT1_I = "CONSTAT1 Imputation Flag"
   CONSTAT2_I = "CONSTAT2 Imputation Flag"
   CONSTAT3_I = "CONSTAT3 Imputation Flag"
   CONSTAT4_I = "CONSTAT4 Imputation Flag"
   PILLR_I = "PILLR Imputation Flag"
   CONDOMR_I = "CONDOMR Imputation Flag"
   SEX1MTHD1_I = "SEX1MTHD1 Imputation Flag"
   SEX1MTHD2_I = "SEX1MTHD2 Imputation Flag"
   SEX1MTHD3_I = "SEX1MTHD3 Imputation Flag"
   SEX1MTHD4_I = "SEX1MTHD4 Imputation Flag"
   MTHUSE12_I = "MTHUSE12 Imputation Flag"
   METH12M1_I = "METH12M1 Imputation Flag"
   METH12M2_I = "METH12M2 Imputation Flag"
   METH12M3_I = "METH12M3 Imputation Flag"
   METH12M4_I = "METH12M4 Imputation Flag"
   MTHUSE3_I = "MTHUSE3 Imputation Flag"
   METH3M1_I = "METH3M1 Imputation Flag"
   METH3M2_I = "METH3M2 Imputation Flag"
   METH3M3_I = "METH3M3 Imputation Flag"
   METH3M4_I = "METH3M4 Imputation Flag"
   FMETHOD1_I = "FMETHOD1 Imputation Flag"
   FMETHOD2_I = "FMETHOD2 Imputation Flag"
   FMETHOD3_I = "FMETHOD3 Imputation Flag"
   FMETHOD4_I = "FMETHOD4 Imputation Flag"
   DATEUSE1_I = "DATEUSE1 Imputation Flag"
   OLDWP01_I = "OLDWP01 Imputation Flag"
   OLDWP02_I = "OLDWP02 Imputation Flag"
   OLDWP03_I = "OLDWP03 Imputation Flag"
   OLDWP04_I = "OLDWP04 Imputation Flag"
   OLDWP05_I = "OLDWP05 Imputation Flag"
   OLDWP06_I = "OLDWP06 Imputation Flag"
   OLDWP07_I = "OLDWP07 Imputation Flag"
   OLDWP08_I = "OLDWP08 Imputation Flag"
   OLDWP09_I = "OLDWP09 Imputation Flag"
   OLDWP10_I = "OLDWP10 Imputation Flag"
   OLDWP11_I = "OLDWP11 Imputation Flag"
   OLDWP12_I = "OLDWP12 Imputation Flag"
   OLDWP13_I = "OLDWP13 Imputation Flag"
   OLDWP14_I = "OLDWP14 Imputation Flag"
   OLDWR01_I = "OLDWR01 Imputation Flag"
   OLDWR02_I = "OLDWR02 Imputation Flag"
   OLDWR03_I = "OLDWR03 Imputation Flag"
   OLDWR04_I = "OLDWR04 Imputation Flag"
   OLDWR05_I = "OLDWR05 Imputation Flag"
   OLDWR06_I = "OLDWR06 Imputation Flag"
   OLDWR07_I = "OLDWR07 Imputation Flag"
   OLDWR08_I = "OLDWR08 Imputation Flag"
   OLDWR09_I = "OLDWR09 Imputation Flag"
   OLDWR10_I = "OLDWR10 Imputation Flag"
   OLDWR11_I = "OLDWR11 Imputation Flag"
   OLDWR12_I = "OLDWR12 Imputation Flag"
   OLDWR13_I = "OLDWR13 Imputation Flag"
   OLDWR14_I = "OLDWR14 Imputation Flag"
   WANTRP01_I = "WANTRP01 Imputation Flag"
   WANTRP02_I = "WANTRP02 Imputation Flag"
   WANTRP03_I = "WANTRP03 Imputation Flag"
   WANTRP04_I = "WANTRP04 Imputation Flag"
   WANTRP05_I = "WANTRP05 Imputation Flag"
   WANTRP06_I = "WANTRP06 Imputation Flag"
   WANTRP07_I = "WANTRP07 Imputation Flag"
   WANTRP08_I = "WANTRP08 Imputation Flag"
   WANTRP09_I = "WANTRP09Imputation Flag"
   WANTRP10_I = "WANTRP10 Imputation Flag"
   WANTRP11_I = "WANTRP11 Imputation Flag"
   WANTRP12_I = "WANTRP12 Imputation Flag"
   WANTRP13_I = "WANTRP13 Imputation Flag"
   WANTRP14_I = "WANTRP14 Imputation Flag"
   WANTP01_I = "WANTP01 Imputation Flag"
   WANTP02_I = "WANTP02 Imputation Flag"
   WANTP03_I = "WANTP03 Imputation Flag"
   WANTP04_I = "WANTP04 Imputation Flag"
   WANTP05_I = "WANTP05 Imputation Flag"
   WANTP06_I = "WANTP06 Imputation Flag"
   WANTP07_I = "WANTP07 Imputation Flag"
   WANTP08_I = "WANTP08 Imputation Flag"
   WANTP09_I = "WANTP09 Imputation Flag"
   WANTP10_I = "WANTP10 Imputation Flag"
   WANTP11_I = "WANTP11 Imputation Flag"
   WANTP12_I = "WANTP12 Imputation Flag"
   WANTP13_I = "WANTP13 Imputation Flag"
   WANTP14_I = "WANTP14 Imputation Flag"
   NWWANTRP01_I = "NWWANTRP01 Imputation Flag"
   NWWANTRP02_I = "NWWANTRP02 Imputation Flag"
   NWWANTRP03_I = "NWWANTRP03 Imputation Flag"
   NWWANTRP04_I = "NWWANTRP04 Imputation Flag"
   NWWANTRP05_I = "NWWANTRP05 Imputation Flag"
   NWWANTRP06_I = "NWWANTRP06 Imputation Flag"
   NWWANTRP07_I = "NWWANTRP07 Imputation Flag"
   NWWANTRP08_I = "NWWANTRP08 Imputation Flag"
   NWWANTRP09_I = "NWWANTRP09 Imputation Flag"
   NWWANTRP10_I = "NWWANTRP10 Imputation Flag"
   NWWANTRP11_I = "NWWANTRP11 Imputation Flag"
   NWWANTRP12_I = "NWWANTRP12 Imputation Flag"
   NWWANTRP13_I = "NWWANTRP13 Imputation Flag"
   NWWANTRP14_I = "NWWANTRP14 Imputation Flag"
   WANTP5_I = "WANTP5 Imputation Flag"
   FPTIT12 = "Type of clinic used for fp services in last 12 months (RECODE)"
   FPTITMED = "Type of clinic used for medical services in last 12 months (RECODE)"
   FPTITSTE = "Source of services in last 12 months: Sterilization operation (RECODE)"
   FPTITBC = "Source of services last 12 mos: Method of BC or prescription (RECODE)"
   FPTITCHK = "Source of services in last 12 mos: Check-up or test re: BC  (RECODE)"
   FPTITCBC = "Source of services in last 12 mos: Counseling re BC (RECODE)"
   FPTITCST = "Source of services in last 12 mos: Counseling re sterilization (RECODE)"
   FPTITEC = "Source of service in last 12 months: EC or prescription (RECODE)"
   FPTITCEC = "Source of service in last 12 mos: Counseling or info on EC (RECODE)"
   FPTITPRE = "Source of service in last 12 mos: Pregnancy test (RECODE)"
   FPTITABO = "Source of service in last 12 mos: Abortion (RECODE)"
   FPTITPAP = "Source of service in last 12 mos: Pap smear (RECODE)"
   FPTITPEL = "Source of service in last 12 mos: Pelvic exam (RECODE)"
   FPTITPRN = "Source of service in last 12 mos: Prenatal care (RECODE)"
   FPTITPPR = "Source of service in last 12 mos: Post-pregnancy care (RECODE)"
   FPTITSTD = "Source of service in last 12 mos: testing for STD (RECODE)"
   FPREGFP = "Title X clinic used for FP svcs: Regular place for care (RECODE)"
   FPREGMED = "Title X clinic used for med svcs: Regular place for care (RECODE)"
   FPTIT12_I = "FPTIT12 Imputation Flag"
   FPTITMED_I = "FPTITMED Imputation Flag"
   FPTITSTE_I = "FPTITSTE Imputation Flag"
   FPTITBC_I = "FPTITBC Imputation Flag"
   FPTITCHK_I = "FPTITCHK Imputation Flag"
   FPTITCBC_I = "FPTITCBC Imputation Flag"
   FPTITCST_I = "FPTITCST Imputation Flag"
   FPTITEC_I = "FPTITEC Imputation Flag"
   FPTITCEC_I = "FPTITCEC Imputation Flag"
   FPTITPRE_I = "FPTITPRE Imputation Flag"
   FPTITABO_I = "FPTITABO Imputation Flag"
   FPTITPAP_I = "FPTITPAP Imputation Flag"
   FPTITPEL_I = "FPTITPEL Imputation Flag"
   FPTITPRN_I = "FPTITPRN Imputation Flag"
   FPTITPPR_I = "FPTITPPR Imputation Flag"
   FPTITSTD_I = "FPTITSTD Imputation Flag"
   FPREGFP_I = "FPREGFP Imputation Flag"
   FPREGMED_I = "FPREGMED Imputation Flag"
   INTENT = "Intentions for additional births (RECODE)"
   ADDEXP = "Central number of additional births expected (RECODE)"
   INTENT_I = "INTENT Imputation Flag"
   ADDEXP_I = "ADDEXP Imputation Flag"
   ANYPRGHP = "Any medical help to become pregnant (RECODE)"
   ANYMSCHP = "Any medical help to prevent miscarriage (RECODE)"
   INFEVER = "Ever used infertility services of any kind (RECODE)"
   OVULATE = "Infertility services: drugs to improve ovulation (RECODE)"
   TUBES = "Infertility services: surgery to correct blocked tubes (RECODE)"
   INFERTR = "Infertility services: Infertility testing on R (RECODE)"
   INFERTH = "Infertility services: Infertility testing on H/P (RECODE)"
   ADVICE = "Infertility services: Advice (RECODE)"
   INSEM = "Infertility services: Artificial insemination (RECODE)"
   INVITRO = "Infertility services: In vitro fertilization or other assisted reproduction (RECODE)"
   ENDOMET = "Infertility services: Surgery or drug treatment for endometriosis (RECODE)"
   FIBROIDS = "Infertility services: Surgery for uterine fibroids (RECODE)"
   PIDTREAT = "Ever been treated for PID (RECODE)"
   EVHIVTST = "Ever had an HIV test (RECODE)"
   FPTITHIV = "Source of service in the last 12 mos: HIV test (RECODE)"
   ANYPRGHP_I = "ANYPRGHP Imputation Flag"
   ANYMSCHP_I = "ANYMSCHP Imputation Flag"
   INFEVER_I = "INFEVER Imputation Flag"
   OVULATE_I = "OVULATE Imputation Flag"
   TUBES_I = "TUBES Imputation Flag"
   INFERTR_I = "INFERTR Imputation Flag"
   INFERTH_I = "INFERTH Imputation Flag"
   ADVICE_I = "ADVICE Imputation Flag"
   INSEM_I = "INSEM Imputation Flag"
   INVITRO_I = "INVITRO Imputation Flag"
   ENDOMET_I = "ENDOMET Imputation Flag"
   FIBROIDS_I = "FIBROIDS Imputation Flag"
   PIDTREAT_I = "PIDTREAT Imputation Flag"
   EVHIVTST_I = "EVHIVTST Imputation Flag"
   FPTITHIV_I = "FPTITHIV Imputation Flag"
   CURR_INS = "Current health insurance status"
   METRO = "Place of residence (metropolitan-non-metropolitan) (RECODE)"
   RELIGION = "Current religious affiliation (RECODE)"
   LABORFOR = "Labor force status (RECODE)"
   CURR_INS_I = "CURR_INS Imputation Flag"
   RELIGION_I = "RELIGION Imputation Flag"
   LABORFOR_I = "LABORFOR Imputation Flag"
   POVERTY = "Poverty level income (RECODE)"
   TOTINCR = "Total income of R's family (RECODE)"
   PUBASSIS = "Whether R received public assistance in last calendar year (RECODE)"
   POVERTY_I = "POVERTY Imputation Flag"
   TOTINCR_I = "TOTINCR Imputation Flag"
   PUBASSIS_I = "PUBASSIS Imputation Flag"
   WGT2017_2019 = "Final weight for the 2017-2019 NSFG"
   SECU = "Randomized version of the sampling error computational unit"
   SEST = "Randomized version of the stratum"
   CMINTVW = "Century month for date of interview (Computed in Flow Check A-1)"
   CMLSTYR = "Century month for month/year of interview minus one year (Computed in Flow Check A-1)"
   CMJAN3YR = "Century month of January Three Years Prior to Year of interview (Computed in Flow Check A-1)"
   CMJAN4YR = "Century month of January Four Years Prior to Year of Interview (Computed in Flow Check A-1)"
   CMJAN5YR = "Century month of January Five Years Prior to Year of Interview (Computed in Flow Check A-1)"
   QUARTER = "Quarter when case was sampled"
   PHASE = "Regular- or double-sample portion of the quarter"
   INTVWYEAR = "Calendar year when interview occurred"
   INTVLNGTH = "Interview Length in Minutes" ;

* SAS FORMAT STATEMENT;

*/
FORMAT
   RSCRNINF Y1N5NALC.       RSCRAGE AGESCRN.         RSCRHISP YESNONAF.    
   RSCRRACE RSCRRACE.       AGE_A AGEFMT.            AGE_R AGEFMT.         
   AGESCRN AGESCRN.         HISP Y1N5RDF.            HISPGRP HISPGRPF.     
   PRIMLANG1 PRIMLANG.      PRIMLANG2 PRIMLANG.      PRIMLANG3 PRIMLANG.   
   ROSCNT ROSCNT.           NUMCHILD ROST4TOP.       HHKIDS18 ROST5TOP.    
   DAUGHT918 DGHT918F.      SON918 SON918F.          NONBIOKIDS ROST2TOP.  
   MARSTAT MARSTAT.         FMARSTAT FMARSTAT.       FMARIT FMARIT.        
   EVRMARRY EVRMARRY.       HPLOCALE HPLOCALE.       MANREL MANRELF.       
   GOSCHOL Y1N5RDF.         VACA Y1N5RDF.            HIGRADE HIGRADE.      
   COMPGRD Y1N5RDF.         DIPGED DIPGED.           EARNHS_Y YEARFMT.     
   HISCHGRD HISCHGRD.       LSTGRADE EDUCAT.         MYSCHOL_Y YEARFMT.    
   HAVEDEG Y1N5RDF.         DEGREES DEGREES.         EARNBA_Y YEARFMT.     
   EXPSCHL Y1N5RDF.         EXPGRADE EXPGRADE2F.     WTHPARNW WTHPARNW.    
   ONOWN Y1N5RDF.           ONOWN18 Y1N5RDF.         INTACT Y1N5RDF.       
   PARMARR Y1N5RDF.         INTACT18 INTACT18F.      LVSIT14F LVSIT14F.    
   LVSIT14M LVSIT14M.       WOMRASDU WOMRASDU.       MOMDEGRE MDDEGRE.     
   MOMWORKD MOMWORKD.       MOMFSTCH MOMFSTCH.       MOM18 MOM18F.         
   MANRASDU MANRASDU.       FOSTEREV Y1N5RDF.        MNYFSTER MNYFSTER.    
   DURFSTER DURFSTER.       AGEFSTER AGEFSTER.       MENARCHE MENARCHE.    
   PREGNOWQ Y1N5RDF.        MAYBPREG MAYBPREG.       EVERPREG Y1N5C.       
   CURRPREG Y1N5C.          MOSCURRP MOSPRGF.        HASBABES Y1N5RDF.     
   CMLASTLB YEARFMT.        CMLSTPRG YEARFMT.        NPLACED KID1PLUS.     
   NDIED KID1PLUS.          NADOPTV KID1PLUS.        TOTPLACD KID1PLUS.    
   OTHERKID Y1N5RDF.        NOTHRKID NOTHRKID.       RELOTHKD RELOTHKD.    
   ADPTOTKD ADPTOTKD.       TRYADOPT Y1N5RDF.        TRYEITHR TRYEITHRF.   
   NBAGECAME1 NBAGECAT.     OTHKDFOS Y1N5RDF.        RELOTHKD2 RELOTHKD.   
   ADPTOTKD2 ADPTOTKD.      TRYADOPT2 Y1N5RDF.       TRYEITHR2 TRYEITHRF.  
   NBAGECAME2 NBAGECAT.     OTHKDFOS2 Y1N5RDF.       RELOTHKD3 RELOTHKD.   
   ADPTOTKD3 ADPTOTKD.      TRYADOPT3 Y1N5RDF.       TRYEITHR3 TRYEITHRF.  
   NBAGECAME3 NBAGECAT.     OTHKDFOS3 Y1N5RDF.       RELOTHKD4 RELOTHKD.   
   ADPTOTKD4 ADPTOTKD.      TRYADOPT4 Y1N5RDF.       TRYEITHR4 TRYEITHRF.  
   NBAGECAME4 NBAGECAT.     OTHKDFOS4 Y1N5RDF.       RELOTHKD5 RELOTHKD.   
   ADPTOTKD5 ADPTOTKD.      TRYADOPT5 Y1N5RDF.       TRYEITHR5 TRYEITHRF.  
   NBAGECAME5 NBAGECAT.     OTHKDFOS5 Y1N5RDF.       RELOTHKD6 RELOTHKD.   
   ADPTOTKD6 ADPTOTKD.      TRYADOPT6 Y1N5RDF.       TRYEITHR6 TRYEITHRF.  
   NBAGECAME6 NBAGECAT.     OTHKDFOS6 Y1N5RDF.       RELOTHKD7 RELOTHKD.   
   ADPTOTKD7 ADPTOTKD.      TRYADOPT7 Y1N5RDF.       TRYEITHR7 TRYEITHRF.  
   NBAGECAME7 NBAGECAT.     OTHKDFOS7 Y1N5RDF.       RELOTHKD8 RELOTHKD.   
   ADPTOTKD8 ADPTOTKD.      TRYADOPT8 Y1N5RDF.       TRYEITHR8 TRYEITHRF.  
   NBAGECAME8 NBAGECAT.     OTHKDFOS8 Y1N5RDF.       RELOTHKD9 RELOTHKD.   
   ADPTOTKD9 ADPTOTKD.      TRYADOPT9 Y1N5RDF.       TRYEITHR9 TRYEITHRF.  
   NBAGECAME9 NBAGECAT.     OTHKDFOS9 Y1N5RDF.       RELOTHKD10 RELOTHKD.  
   ADPTOTKD10 ADPTOTKD.     TRYADOPT10 Y1N5RDF.      TRYEITHR10 TRYEITHRF. 
   NBAGECAME10 NBAGECAT.    OTHKDFOS10 Y1N5RDF.      RELOTHKD11 RELOTHKD.  
   ADPTOTKD11 ADPTOTKD.     TRYADOPT11 Y1N5RDF.      TRYEITHR11 TRYEITHRF. 
   NBAGECAME11 NBAGECAT.    OTHKDFOS11 Y1N5RDF.      RELOTHKD12 RELOTHKD.  
   ADPTOTKD12 ADPTOTKD.     TRYADOPT12 Y1N5RDF.      TRYEITHR12 TRYEITHRF. 
   NBAGECAME12 NBAGECAT.    OTHKDFOS12 Y1N5RDF.      RELOTHKD13 RELOTHKD.  
   ADPTOTKD13 ADPTOTKD.     TRYADOPT13 Y1N5RDF.      TRYEITHR13 TRYEITHRF. 
   NBAGECAME13 NBAGECAT.    OTHKDFOS13 Y1N5RDF.      RELOTHKD14 RELOTHKD.  
   ADPTOTKD14 ADPTOTKD.     TRYADOPT14 Y1N5RDF.      TRYEITHR14 TRYEITHRF. 
   NBAGECAME14 NBAGECAT.    OTHKDFOS14 Y1N5RDF.      RELOTHKD15 RELOTHKD.  
   ADPTOTKD15 ADPTOTKD.     TRYADOPT15 Y1N5RDF.      TRYEITHR15 TRYEITHRF. 
   NBAGECAME15 NBAGECAT.    OTHKDFOS15 Y1N5RDF.      RELOTHKD16 RELOTHKD.  
   ADPTOTKD16 ADPTOTKD.     TRYADOPT16 Y1N5RDF.      TRYEITHR16 TRYEITHRF. 
   NBAGECAME16 NBAGECAT.    OTHKDFOS16 Y1N5RDF.      RELOTHKD17 RELOTHKD.  
   ADPTOTKD17 ADPTOTKD.     TRYADOPT17 Y1N5RDF.      TRYEITHR17 TRYEITHRF. 
   NBAGECAME17 NBAGECAT.    OTHKDFOS17 Y1N5RDF.      RELOTHKD18 RELOTHKD.  
   ADPTOTKD18 ADPTOTKD.     TRYADOPT18 Y1N5RDF.      TRYEITHR18 TRYEITHRF. 
   NBAGECAME18 NBAGECAT.    OTHKDFOS18 Y1N5RDF.      EVERADPT EVERADPT.    
   SEEKADPT Y1N5RDF.        CONTAGEM Y1N5RDF.        TRYLONG TRYLONG.      
   KNOWADPT Y1N5RDF.        EVWNTANO Y1N5RDF.        EVCONTAG Y1N5RDF.     
   TURNDOWN TURNDOWN.       YQUITTRY YQUITTRY.       APROCESS1 APROCESSF.  
   APROCESS2 APROCESSF.     APROCESS3 APROCESSF.     HRDEMBRYO Y1N5RDF.    
   SRCEMBRYO_1 SRCEMBRYO.   SRCEMBRYO_2 SRCEMBRYO.   SRCEMBRYO_3 SRCEMBRYO.
   SRCEMBRYO_4 SRCEMBRYO.   SRCEMBRYO_5 SRCEMBRYO.   TIMESMAR TIMESMAR.    
   HSBVERIF Y1N5RDF.        HXAGEMAR AGEHP.          LVTOGHX Y1N5RDF.      
   STRTOGHX_Y YEARFMT.      ENGAGHX ENGAGF.          HSBRACE1 RACEFMTD.    
   HSBNRACE1 NRACED.        CHEDMARN EDUCFMT.        MARBEFHX Y1N5RDF.     
   KIDSHX Y1N5RDF.          NUMKDSHX NUMKDS.         KIDLIVHX Y1N5RDF.     
   CHKID18A Y1N5RDF.        CHKID18B KID18BF.        WHRCHKDS1 WHRKDSF.    
   WHRCHKDS2 WHRKDSF.       WHRCHKDS3 WHRKDSF.       SUPPORCH Y1N5JPC.     
   BIOHUSBX Y1N5RDF.        BIONUMHX BIONUMFF.       HXAGEMAR2 AGEHP.      
   LVTOGHX2 Y1N5RDF.        STRTOGHX_Y2 YEARFMT.     ENGAGHX2 ENGAGF.      
   HSBRACE2 RACEFMTD.       HSBNRACE2 NRACED.        CHEDMARN2 EDUCFMT.    
   MARBEFHX2 Y1N5RDF.       KIDSHX2 Y1N5RDF.         NUMKDSHX2 NUMKDS.     
   KIDLIVHX2 Y1N5RDF.       CHKID18A2 Y1N5RDF.       CHKID18B2 KID18BF.    
   WHRCHKDS5 WHRKDSF.       WHRCHKDS6 WHRKDSF.       SUPPORCH2 Y1N5JPC.    
   BIOHUSBX2 Y1N5RDF.       BIONUMHX2 BIONUMFF.      HXAGEMAR3 AGEHP.      
   LVTOGHX3 Y1N5RDF.        STRTOGHX_Y3 YEARFMT.     ENGAGHX3 ENGAGF.      
   HSBRACE3 RACEFMTD.       HSBNRACE3 NRACED.        CHEDMARN3 EDUCFMT.    
   MARBEFHX3 Y1N5RDF.       KIDSHX3 Y1N5RDF.         NUMKDSHX3 NUMKDS.     
   KIDLIVHX3 Y1N5RDF.       CHKID18A3 Y1N5RDF.       CHKID18B3 KID18BF.    
   WHRCHKDS9 WHRKDSF.       WHRCHKDS10 WHRKDSF.      SUPPORCH3 Y1N5JPC.    
   BIOHUSBX3 Y1N5RDF.       BIONUMHX3 BIONUMFF.      HXAGEMAR4 AGEHP.      
   LVTOGHX4 Y1N5RDF.        STRTOGHX_Y4 YEARFMT.     ENGAGHX4 ENGAGF.      
   HSBRACE4 RACEFMTD.       HSBNRACE4 NRACED.        CHEDMARN4 EDUCFMT.    
   MARBEFHX4 Y1N5RDF.       KIDSHX4 Y1N5RDF.         NUMKDSHX4 NUMKDS.     
   KIDLIVHX4 Y1N5RDF.       CHKID18A4 Y1N5RDF.       CHKID18B4 KID18BF.    
   WHRCHKDS13 WHRKDSF.      WHRCHKDS14 WHRKDSF.      SUPPORCH4 Y1N5JPC.    
   BIOHUSBX4 Y1N5RDF.       BIONUMHX4 BIONUMFF.      CMMARRCH YEARFMT.     
   PREVHUSB PREVHUSB.       WNSTRTCP_Y YEARFMT.      CPHERAGE AGERFEMC.    
   CPHISAGE AGEHP.          CPENGAG1 ENGAGF.         WILLMARR DEFPROBF.    
   CURCOHRACE RACEFMTD.     CURCOHNRACE NRACED.      CPEDUC EDUCFMT.       
   CPMARBEF Y1N5RDF.        CPKIDS Y1N5RDF.          CPNUMKDS NUMKDS.      
   CPKIDLIV Y1N5RDF.        CPKID18A Y1N5RDF.        CPKID18B KID18BF.     
   WHRCPKDS1 WHRKDSF.       WHRCPKDS2 WHRKDSF.       WHRCPKDS3 WHRKDSF.    
   SUPPORCP Y1N5JPC.        BIOCP Y1N5RDF.           BIONUMCP BIONUMFF.    
   CMSTRTHP YEARFMT.        LIVEOTH Y1N5RDF.         EVRCOHAB Y1N5C.       
   HMOTHMEN HMOTHMEN.       PREVCOHB PREVCOHB.       STRTOTHX_Y YEARFMT.   
   HERAGECX AGERFEMC.       HISAGECX AGEHPNA.        ENGAG1CX ENGAGF.      
   COH1RACE RACEFMTD.       COH1NRACE NRACED.        MAREVCX Y1N5RDF.      
   CXKIDS Y1N5RDF.          BIOFCPX Y1N5RDF.         BIONUMCX BIONUMFF.    
   STPTOGCX_Y YEARFMT.      STRTOTHX_Y2 YEARFMT.     HERAGECX2 AGERFEMC.   
   HISAGECX2 AGEHP.         ENGAG1CX2 ENGAGF.        MAREVCX2 Y1N5RDF.     
   CXKIDS2 Y1N5RDF.         BIOFCPX2 Y1N5RDF.        BIONUMCX2 BIONUMFF.   
   STPTOGCX_Y2 YEARFMT.     STRTOTHX_Y3 YEARFMT.     HERAGECX3 AGERFEMC.   
   HISAGECX3 AGEHP.         ENGAG1CX3 ENGAGF.        MAREVCX3 Y1N5RDF.     
   CXKIDS3 Y1N5RDF.         BIOFCPX3 Y1N5RDF.        BIONUMCX3 BIONUMFF.   
   STPTOGCX_Y3 YEARFMT.     STRTOTHX_Y4 YEARFMT.     HERAGECX4 AGERFEMC.   
   HISAGECX4 AGEHP.         ENGAG1CX4 ENGAGF.        MAREVCX4 Y1N5RDF.     
   CXKIDS4 Y1N5RDF.         BIOFCPX4 Y1N5RDF.        BIONUMCX4 BIONUMFF.   
   STPTOGCX_Y4 YEARFMT.     COHCHANCE DEFPROBF.      MARRCHANCE DEFPROBF.  
   PMARCOH DEFPROBF.        EVERSEX Y1N5RDF.         RHADSEX Y1N5C.        
   YNOSEX YNOSEX.           GRFSTSX GRFSTSXF.        SXMTONCE Y1N5RDF.     
   TALKPAR1 TALKPARF.       TALKPAR2 TALKPARF.       TALKPAR3 TALKPARF.    
   TALKPAR4 TALKPARF.       TALKPAR5 TALKPARF.       TALKPAR6 TALKPARF.    
   TALKPAR7 TALKPARF.       SEDNO Y1N5RDF.           SEDNOLC1 SEDFMT.      
   SEDNOLC2 SEDFMT.         SEDNOLC3 SEDFMT.         SEDNOLC4 SEDFMT.      
   SEDNOG SXEDGRF.          SEDNOSX BFAFTF.          SEDBC Y1N5RDF.        
   SEDBCLC1 SXEDWHRINS.     SEDBCLC2 SXEDWHRINS.     SEDBCLC3 SXEDWHRINS.  
   SEDBCLC4 SXEDWHRINS.     SEDBCG SXEDGRF.          SEDBCSX BFAFTF.       
   SEDWHBC Y1N5RDF.         SEDWHLC1 SEDFMT.         SEDWHLC2 SEDFMT.      
   SEDWHLC3 SEDFMT.         SEDWHLC4 SEDFMT.         SEDWHBCG SXEDGRF.     
   SEDWBCSX BFAFTF.         SEDCOND Y1N5RDF.         SEDCONLC1 SEDFMT.     
   SEDCONLC2 SEDFMT.        SEDCONLC3 SEDFMT.        SEDCONLC4 SEDFMT.     
   SEDCONDG SXEDGRF.        SEDCONSX BFAFTF.         SEDSTD Y1N5RDF.       
   SEDSTDLC1 SEDFMT.        SEDSTDLC2 SEDFMT.        SEDSTDLC3 SEDFMT.     
   SEDSTDLC4 SEDFMT.        SEDSTDG SXEDGRF.         SEDSTDSX BFAFTF.      
   SEDHIV Y1N5RDF.          SEDHIVLC1 SEDFMT.        SEDHIVLC2 SEDFMT.     
   SEDHIVLC3 SEDFMT.        SEDHIVLC4 SEDFMT.        SEDHIVG SXEDGRF.      
   SEDHIVSX BFAFTF.         SEDABST Y1N5RDF.         SEDABLC1 SXEDWHRINS.  
   SEDABLC2 SXEDWHRINS.     SEDABLC3 SXEDWHRINS.     SEDABLC4 SXEDWHRINS.  
   SEDABSTG SXEDGRF.        SEDABSSX BFAFTF.         SAMEMAN Y1N5RDF.      
   WHOFSTPR MTCHMANF.       KNOWFP FSEXREL.          STILFPSX Y1N5RDF.     
   LSTSEXFP_M LSTSEXFPF.    LSTSEXFP_Y YEARFMT.      CMLSEXFP CMLSEXFP.    
   CMFPLAST CMFMT.          FPOTHREL FSEXREL.        FPEDUC EDUCFMT.       
   FSEXRACE RACEFMTD.       FSEXNRACE NRACED.        FPRN CURRREL.         
   WHICH1ST WHICH1ST.       SEXAFMEN Y1N5RDF.        PTSB4MAR PTSB4MAR.    
   WHOSNC1Y Y1N5RDF.        MATCHFP Y1N5RDF.         MATCHHP MTCHMANF.     
   P1YRELP P1YRELP.         CMLSEX CMFMT.            P1YLSEX_M MNTHFMT.    
   P1YLSEX_Y YEARFMT.       P1YCURRP Y1N5RDF.        PCURRNT Y1N5C.        
   MATCHFP2 Y1N5RDF.        MATCHHP2 MTCHMANF.       P1YRELP2 P1YRELP.     
   CMLSEX2 CMFMT.           P1YLSEX_M2 MNTHFMT.      P1YLSEX_Y2 YEARFMT.   
   P1YCURRP2 Y1N5RDF.       PCURRNT2 Y1N5C.          MATCHFP3 Y1N5RDF.     
   MATCHHP3 MTCHMANF.       P1YRELP3 P1YRELP.        CMLSEX3 CMFMT.        
   P1YLSEX_M3 MNTHFMT.      P1YLSEX_Y3 YEARFMT.      P1YCURRP3 Y1N5RDF.    
   PCURRNT3 Y1N5C.          P1YOTHREL FSEXREL.       P1YRAGE P1YRAGE.      
   P1YHSAGE P1YHSAGE.       P1YRF FSEXREL.           P1YFSEX_M P1YFSEXF.   
   P1YFSEX_Y YEARFMT.       CMFSEX CMFSEX.           CMFSEXTOT CMFMT.      
   P1YEDUC EDUCFMT.         P1YRACE1 RACEFMTD.       P1YNRACE1 NRACED.     
   P1YRN CURRREL.           P1YOTHREL2 FSEXREL.      P1YRAGE2 P1YRAGE.     
   P1YHSAGE2 P1YHSAGE.      P1YRF2 FSEXREL.          P1YFSEX_M2 P1YFSEXF.  
   P1YFSEX_Y2 YEARFMT.      CMFSEX2 CMFSEX.          CMFSEXTOT2 CMFMT.     
   P1YEDUC2 EDUCFMT.        P1YRACE2 RACEFMTD.       P1YNRACE2 NRACED.     
   P1YRN2 CURRREL.          P1YOTHREL3 FSEXREL.      P1YRAGE3 P1YRAGE.     
   P1YHSAGE3 P1YHSAGE.      P1YRF3 FSEXREL.          P1YFSEX_M3 P1YFSEXF.  
   P1YFSEX_Y3 YEARFMT.      CMFSEX3 CMFSEX.          CMFSEXTOT3 CMFMT.     
   P1YEDUC3 EDUCFMT.        P1YRACE3 RACEFMTD.       P1YNRACE3 NRACED.     
   P1YRN3 CURRREL.          CURRPRTT CURRPRTS.       CURRPRTS CURRPRTS.    
   CMCURRP1 CMFMT.          CMCURRP2 CMFMT.          CMCURRP3 CMFMT.       
   EVERTUBS EVERTUBS.       ESSURE Y1N5RDF.          EVERHYST Y1N5RDF.     
   EVEROVRS Y1N5RDF.        EVEROTHR Y1N5RDF.        WHTOOPRC WHTOOPRC.    
   ONOTFUNC Y1N5RDF.        DFNLSTRL Y1N5RDF.        ANYTUBAL Y1N5C.       
   HYST Y1N5C.              OVAREM Y1N5C.            OTHR Y1N5C.           
   ANYFSTER Y1N5C.          ANYOPSMN Y1N5RDF.        WHATOPSM WHATOPSM.    
   DFNLSTRM Y1N5RDF.        ANYMSTER Y1N5C.          ANYVAS Y1N5C.         
   OTHRM Y1N2RECF.          DATFEMOP_Y YEARFMT.      PLCFEMOP PLCOPF.      
   PAYRSTER1 PAYFMT2F.      PAYRSTER2 PAYFMT2F.      PAYRSTER3 PAYFMT2F.   
   RHADALL Y1N5RDF.         HHADALL HHADALL.         FMEDREAS1 FMEDREASF.  
   FMEDREAS2 FMEDREASF.     FMEDREAS3 FMEDREASF.     FMEDREAS4 FMEDREASF.  
   FMEDREAS5 FMEDREASF.     BCREAS BCREAS.           BCWHYF BCWHYFF.       
   MINCDNNR MINCDNF.        DATFEMOP_Y2 YEARFMT.     PLCFEMOP2 PLCOPF.     
   PAYRSTER6 PAYFMT2F.      PAYRSTER7 PAYFMT2F.      PAYRSTER8 PAYFMT2F.   
   RHADALL2 Y1N5RDF.        HHADALL2 HHADALL.        FMEDREAS7 FMEDREASF.  
   FMEDREAS8 FMEDREASF.     FMEDREAS9 FMEDREASF.     FMEDREAS10 FMEDREASF. 
   FMEDREAS11 FMEDREASF.    BCREAS2 BCREAS.          BCWHYF2 BCWHYFF.      
   MINCDNNR2 MINCDNF.       DATFEMOP_Y3 YEARFMT.     PLCFEMOP3 PLCOPF.     
   PAYRSTER11 PAYFMT2F.     PAYRSTER12 PAYFMT2F.     PAYRSTER13 PAYFMT2F.  
   RHADALL3 Y1N5RDF.        HHADALL3 HHADALL.        FMEDREAS13 FMEDREASF. 
   FMEDREAS14 FMEDREASF.    FMEDREAS15 FMEDREASF.    FMEDREAS16 FMEDREASF. 
   FMEDREAS17 FMEDREASF.    BCREAS3 BCREAS.          BCWHYF3 BCWHYFF.      
   MINCDNNR3 MINCDNF.       DATFEMOP_Y4 YEARFMT.     PLCFEMOP4 PLCOPF.     
   PAYRSTER16 PAYFMT2F.     PAYRSTER17 PAYFMT2F.     PAYRSTER18 PAYFMT2F.  
   RHADALL4 Y1N5RDF.        HHADALL4 HHADALL.        FMEDREAS19 FMEDREASF. 
   FMEDREAS20 FMEDREASF.    FMEDREAS21 FMEDREASF.    FMEDREAS22 FMEDREASF. 
   FMEDREAS23 FMEDREASF.    BCREAS4 BCREAS.          BCWHYF4 BCWHYFF.      
   MINCDNNR4 MINCDNF.       OPERSAME1 Y1N5RDF.       OPERSAME2 Y1N5RDF.    
   OPERSAME3 Y1N5RDF.       OPERSAME4 Y1N5RDF.       OPERSAME5 Y1N5RDF.    
   OPERSAME6 Y1N5RDF.       DATEOPMN_Y YEARFMT.      WITHIMOP Y1N5RDF.     
   VASJAN4YR Y1N5RDF.       PLACOPMN PLCOPF.         PAYMSTER1 PAYFMT2F.   
   PAYMSTER2 PAYFMT2F.      RHADALLM Y1N5RDF.        HHADALLM Y1N5RDF.     
   MEDREAS1 MEDREASF.       MEDREAS2 MEDREASF.       BCREASM BCREAS.       
   BCWHYM BCWHYFF.          MINCDNMN MINCDNF.        REVSTUBL Y1N5RDF.     
   DATRVSTB_Y YEARFMT.      REVSVASX Y1N5RDF.        DATRVVEX_Y YEARFMT.   
   TUBS Y1N5C.              VASECT Y1N5C.            RSURGSTR Y1N5C.       
   PSURGSTR Y1N5C.          ONLYTBVS Y1N5C.          RWANTRVT DFPRBNAF.    
   MANWANTT DEFPROBF.       RWANTREV DEFPROBF.       MANWANTR DEFPROBF.    
   POSIBLPG Y1N5RDF.        REASIMPR REASR.          POSIBLMN Y1N5RDF.     
   REASIMPP REASP.          CANHAVER Y1N5RDF.        REASDIFF1 REASDIFFF.  
   REASDIFF2 REASDIFFF.     REASDIFF3 REASDIFFF.     REASDIFF4 REASDIFFF.  
   REASDIFF5 REASDIFFF.     CANHAVEM Y1N5RDF.        PREGNONO Y1N5RDF.     
   REASNONO1 REASNONOF.     REASNONO2 REASNONOF.     REASNONO3 REASNONOF.  
   RSTRSTAT RSTRSTAT.       PSTRSTAT RSTRSTAT.       LASTPER LASTPERF.     
   TRYPREG12 Y1N5RDF.       PILL Y1N5RDF.            CONDOM Y1N5RDF.       
   VASECTMY Y1N5RDF.        DEPOPROV Y1N5RDF.        WIDRAWAL Y1N5RDF.     
   RHYTHM Y1N5RDF.          SDAYCBDS Y1N5RDF.        TEMPSAFE Y1N5RDF.     
   PATCH Y1N5RDF.           RING Y1N5RDF.            MORNPILL Y1N5RDF.     
   ECTIMESX ONETWO2F.       ECREASON1 ECREASONF.     ECREASON2 ECREASONF.  
   ECREASON3 ECREASONF.     ECRX ECRX.               ECWHERE PLACE1FMT.    
   ECWHEN ECWHEN.           OTHRMETH01 OTHRMETHF.    OTHRMETH02 OTHRMETHF. 
   OTHRMETH03 OTHRMETHF.    OTHRMETH04 OTHRMETHF.    OTHRMETH05 OTHRMETHF. 
   OTHRMETH06 OTHRMETHF.    OTHRMETH07 OTHRMETHF.    EVIUDTYP1 IUDTYPE.    
   EVIUDTYP2 IUDTYPE.       EVIUDTYP3 IUDTYPE.       NEWMETH NEWMETHF.     
   EVERUSED YESNONAF.       METHDISS Y1N5RDF.        METHSTOP01 METHSTOPF. 
   METHSTOP02 METHSTOPF.    METHSTOP03 METHSTOPF.    METHSTOP04 METHSTOPF. 
   METHSTOP05 METHSTOPF.    METHSTOP06 METHSTOPF.    METHSTOP07 METHSTOPF. 
   METHSTOP08 METHSTOPF.    METHSTOP09 METHSTOPF.    METHSTOP10 METHSTOPF. 
   WHENPILL WHENMTHF.       REASPILL01 REASMFMT.     REASPILL02 REASMFMT.  
   REASPILL03 REASMFMT.     REASPILL04 REASMFMT.     REASPILL05 REASMFMT.  
   REASPILL06 REASMFMT.     REASPILL07 REASMFMT.     REASPILL08 REASMFMT.  
   REASPILL09 REASMFMT.     STOPPILL1 STOPPILLFMT.   STOPPILL2 STOPPILLFMT.
   STOPPILL3 STOPPILLFMT.   STOPPILL4 STOPPILLFMT.   STOPPILL5 STOPPILLFMT.
   STOPPILL6 STOPPILLFMT.   WHENCOND WHENMTHF.       REASCOND01 REASMFMT.  
   REASCOND02 REASMFMT.     REASCOND03 REASMFMT.     REASCOND04 REASMFMT.  
   REASCOND05 REASMFMT.     REASCOND06 REASMFMT.     REASCOND07 REASMFMT.  
   STOPCOND1 STOPCONDFMT.   STOPCOND2 STOPCONDFMT.   WHENIUD WHENMTHF.     
   TYPEIUD_1 IUDTYPE.       TYPEIUD_2 IUDTYPE.       TYPEIUD_3 IUDTYPE.    
   REASIUD01 REASMFMT.      REASIUD02 REASMFMT.      REASIUD03 REASMFMT.   
   REASIUD04 REASMFMT.      REASIUD05 REASMFMT.      REASIUD06 REASMFMT.   
   REASIUD07 REASMFMT.      REASIUD08 REASMFMT.      STOPIUD1 STOPIUDFMT.  
   STOPIUD2 STOPIUDFMT.     STOPIUD3 STOPIUDFMT.     STOPIUD4 STOPIUDFMT.  
   STOPIUD5 STOPIUDFMT.     LARC10 Y1N5RDF.          LARCREMV Y1N5RDF.     
   REMOVWHY_1 REMOVWHYF.    REMOVWHY_2 REMOVWHYF.    REMOVWHY_3 REMOVWHYF. 
   REMOVWHY_4 REMOVWHYF.    REMOVWHY_5 REMOVWHYF.    REMOVDIF Y1N5RDF.     
   REMVDIFY_1 REMVDIFY.     REMVDIFY_2 REMVDIFY.     REMVDIFY_3 REMVDIFY.  
   FIRSMETH1 FIRSMETHF.     FIRSMETH2 FIRSMETHF.     FIRSMETH3 FIRSMETHF.  
   FIRSMETH4 FIRSMETHF.     NUMFIRSM ONETWOF.        DRUGDEV Y1N52D.       
   DRUGDEV2 Y1N52D.         DRUGDEV3 Y1N52D.         DRUGDEV4 Y1N52D.      
   FIRSTIME1 FIRSTIME1F.    FIRSTIME2 FIRSTIME2F.    USEFSTSX USEFSTSX.    
   CMFIRSM CMFMT.           MTHFSTSX1 FIRSMETHF.     MTHFSTSX2 FIRSMETHF.  
   MTHFSTSX3 FIRSMETHF.     MTHFSTSX4 FIRSMETHF.     WNFSTUSE_M MNTHFMT.   
   WNFSTUSE_Y YEARFMT.      CMFSTUSE CMFMT.          FMETHPRS METHPRSF.    
   FMETHPRS2 METHPRSF.      FMETHPRS3 METHPRSF.      FMETHPRS4 METHPRSF.   
   AGEFSTUS AGE44NRDF.      PLACGOTF PLACE1FMT.      PLACGOTF2 PLACE1FMT.  
   PLACGOTF3 PLACE1FMT.     PLACGOTF4 PLACE1FMT.     USEFRSTS Y1N5RDF.     
   MTHFRSTS1 FIRSMETHF.     MTHFRSTS2 FIRSMETHF.     MTHFRSTS3 FIRSMETHF.  
   MTHFRSTS4 FIRSMETHF.     INTR_EC3 Y1N5RDF.        MONSX Y1N5RDF.        
   MONSX2 Y1N5RDF.          MONSX3 Y1N5RDF.          MONSX4 Y1N5RDF.       
   MONSX5 Y1N5RDF.          MONSX6 Y1N5RDF.          MONSX7 Y1N5RDF.       
   MONSX8 Y1N5RDF.          MONSX9 Y1N5RDF.          MONSX10 Y1N5RDF.      
   MONSX11 Y1N5RDF.         MONSX12 Y1N5RDF.         MONSX13 Y1N5RDF.      
   MONSX14 Y1N5RDF.         MONSX15 Y1N5RDF.         MONSX16 Y1N5RDF.      
   MONSX17 Y1N5RDF.         MONSX18 Y1N5RDF.         MONSX19 Y1N5RDF.      
   MONSX20 Y1N5RDF.         MONSX21 Y1N5RDF.         MONSX22 Y1N5RDF.      
   MONSX23 Y1N5RDF.         MONSX24 Y1N5RDF.         MONSX25 Y1N5RDF.      
   MONSX26 Y1N5RDF.         MONSX27 Y1N5RDF.         MONSX28 Y1N5RDF.      
   MONSX29 Y1N5RDF.         MONSX30 Y1N5RDF.         MONSX31 Y1N5RDF.      
   MONSX32 Y1N5RDF.         MONSX33 Y1N5RDF.         MONSX34 Y1N5RDF.      
   MONSX35 Y1N5RDF.         MONSX36 Y1N5RDF.         MONSX37 Y1N5RDF.      
   MONSX38 Y1N5RDF.         MONSX39 Y1N5RDF.         MONSX40 Y1N5RDF.      
   MONSX41 Y1N5RDF.         MONSX42 Y1N5RDF.         MONSX43 Y1N5RDF.      
   MONSX44 Y1N5RDF.         MONSX45 Y1N5RDF.         MONSX46 Y1N5RDF.      
   MONSX47 Y1N5RDF.         MONSX48 Y1N5RDF.         CMSTRTMC CMFMT.       
   CMENDMC CMFMT.           INTR_ED4A Y1N5RDF.       MC1MONS1 MCMONS.      
   MC1SIMSQ MC1SIMSQ.       MC1MONS2 MCMONS.         MC1MONS3 MCMONS.      
   DATBEGIN_M MNTHFMT.      DATBEGIN_Y YEARFMT.      CMDATBEGIN CMFMT.     
   CURRMETH1 METHHXF.       CURRMETH2 METHHXF.       CURRMETH3 METHHXF.    
   CURRMETH4 METHHXF.       LSTMONMETH1 METHHXF.     LSTMONMETH2 METHHXF.  
   LSTMONMETH3 METHHXF.     LSTMONMETH4 METHHXF.     PILL_12 METHYNF.      
   DIAPH_12 METHYNF.        IUD_12 METHYNF.          IMPLANT_12 METHYNF.   
   DEPO_12 METHYNF.         CERVC_12 METHYNF.        MPILL_12 METHYNF.     
   PATCH_12 METHYNF.        RING_12 METHYNF.         METHX1 METHHXF.       
   METHX2 METHHXF.          METHX3 METHHXF.          METHX4 METHHXF.       
   METHX5 METHHXF.          METHX6 METHHXF.          METHX7 METHHXF.       
   METHX8 METHHXF.          METHX9 METHHXF.          METHX10 METHHXF.      
   METHX11 METHHXF.         METHX12 METHHXF.         METHX13 METHHXF.      
   METHX14 METHHXF.         METHX15 METHHXF.         METHX16 METHHXF.      
   METHX17 METHHXF.         METHX18 METHHXF.         METHX19 METHHXF.      
   METHX20 METHHXF.         METHX21 METHHXF.         METHX22 METHHXF.      
   METHX23 METHHXF.         METHX24 METHHXF.         METHX25 METHHXF.      
   METHX26 METHHXF.         METHX27 METHHXF.         METHX28 METHHXF.      
   METHX29 METHHXF.         METHX30 METHHXF.         METHX31 METHHXF.      
   METHX32 METHHXF.         METHX33 METHHXF.         METHX34 METHHXF.      
   METHX35 METHHXF.         METHX36 METHHXF.         METHX37 METHHXF.      
   METHX38 METHHXF.         METHX39 METHHXF.         METHX40 METHHXF.      
   METHX41 METHHXF.         METHX42 METHHXF.         METHX43 METHHXF.      
   METHX44 METHHXF.         METHX45 METHHXF.         METHX46 METHHXF.      
   METHX47 METHHXF.         METHX48 METHHXF.         METHX49 METHHXF.      
   METHX50 METHHXF.         METHX51 METHHXF.         METHX52 METHHXF.      
   METHX53 METHHXF.         METHX54 METHHXF.         METHX55 METHHXF.      
   METHX56 METHHXF.         METHX57 METHHXF.         METHX58 METHHXF.      
   METHX59 METHHXF.         METHX60 METHHXF.         METHX61 METHHXF.      
   METHX62 METHHXF.         METHX63 METHHXF.         METHX64 METHHXF.      
   METHX65 METHHXF.         METHX66 METHHXF.         METHX67 METHHXF.      
   METHX68 METHHXF.         METHX69 METHHXF.         METHX70 METHHXF.      
   METHX71 METHHXF.         METHX72 METHHXF.         METHX73 METHHXF.      
   METHX74 METHHXF.         METHX75 METHHXF.         METHX76 METHHXF.      
   METHX77 METHHXF.         METHX78 METHHXF.         METHX79 METHHXF.      
   METHX80 METHHXF.         METHX81 METHHXF.         METHX82 METHHXF.      
   METHX83 METHHXF.         METHX84 METHHXF.         METHX85 METHHXF.      
   METHX86 METHHXF.         METHX87 METHHXF.         METHX88 METHHXF.      
   METHX89 METHHXF.         METHX90 METHHXF.         METHX91 METHHXF.      
   METHX92 METHHXF.         METHX93 METHHXF.         METHX94 METHHXF.      
   METHX95 METHHXF.         METHX96 METHHXF.         METHX97 METHHXF.      
   METHX98 METHHXF.         METHX99 METHHXF.         METHX100 METHHXF.     
   METHX101 METHHXF.        METHX102 METHHXF.        METHX103 METHHXF.     
   METHX104 METHHXF.        METHX105 METHHXF.        METHX106 METHHXF.     
   METHX107 METHHXF.        METHX108 METHHXF.        METHX109 METHHXF.     
   METHX110 METHHXF.        METHX111 METHHXF.        METHX112 METHHXF.     
   METHX113 METHHXF.        METHX114 METHHXF.        METHX115 METHHXF.     
   METHX116 METHHXF.        METHX117 METHHXF.        METHX118 METHHXF.     
   METHX119 METHHXF.        METHX120 METHHXF.        METHX121 METHHXF.     
   METHX122 METHHXF.        METHX123 METHHXF.        METHX124 METHHXF.     
   METHX125 METHHXF.        METHX126 METHHXF.        METHX127 METHHXF.     
   METHX128 METHHXF.        METHX129 METHHXF.        METHX130 METHHXF.     
   METHX131 METHHXF.        METHX132 METHHXF.        METHX133 METHHXF.     
   METHX134 METHHXF.        METHX135 METHHXF.        METHX136 METHHXF.     
   METHX137 METHHXF.        METHX138 METHHXF.        METHX139 METHHXF.     
   METHX140 METHHXF.        METHX141 METHHXF.        METHX142 METHHXF.     
   METHX143 METHHXF.        METHX144 METHHXF.        METHX145 METHHXF.     
   METHX146 METHHXF.        METHX147 METHHXF.        METHX148 METHHXF.     
   METHX149 METHHXF.        METHX150 METHHXF.        METHX151 METHHXF.     
   METHX152 METHHXF.        METHX153 METHHXF.        METHX154 METHHXF.     
   METHX155 METHHXF.        METHX156 METHHXF.        METHX157 METHHXF.     
   METHX158 METHHXF.        METHX159 METHHXF.        METHX160 METHHXF.     
   METHX161 METHHXF.        METHX162 METHHXF.        METHX163 METHHXF.     
   METHX164 METHHXF.        METHX165 METHHXF.        METHX166 METHHXF.     
   METHX167 METHHXF.        METHX168 METHHXF.        METHX169 METHHXF.     
   METHX170 METHHXF.        METHX171 METHHXF.        METHX172 METHHXF.     
   METHX173 METHHXF.        METHX174 METHHXF.        METHX175 METHHXF.     
   METHX176 METHHXF.        METHX177 METHHXF.        METHX178 METHHXF.     
   METHX179 METHHXF.        METHX180 METHHXF.        METHX181 METHHXF.     
   METHX182 METHHXF.        METHX183 METHHXF.        METHX184 METHHXF.     
   METHX185 METHHXF.        METHX186 METHHXF.        METHX187 METHHXF.     
   METHX188 METHHXF.        METHX189 METHHXF.        METHX190 METHHXF.     
   METHX191 METHHXF.        METHX192 METHHXF.        NUMMULTX1 NUMMULTF.   
   NUMMULTX2 NUMMULTF.      NUMMULTX3 NUMMULTF.      NUMMULTX4 NUMMULTF.   
   NUMMULTX5 NUMMULTF.      NUMMULTX6 NUMMULTF.      NUMMULTX7 NUMMULTF.   
   NUMMULTX8 NUMMULTF.      NUMMULTX9 NUMMULTF.      NUMMULTX10 NUMMULTF.  
   NUMMULTX11 NUMMULTF.     NUMMULTX12 NUMMULTF.     NUMMULTX13 NUMMULTF.  
   NUMMULTX14 NUMMULTF.     NUMMULTX15 NUMMULTF.     NUMMULTX16 NUMMULTF.  
   NUMMULTX17 NUMMULTF.     NUMMULTX18 NUMMULTF.     NUMMULTX19 NUMMULTF.  
   NUMMULTX20 NUMMULTF.     NUMMULTX21 NUMMULTF.     NUMMULTX22 NUMMULTF.  
   NUMMULTX23 NUMMULTF.     NUMMULTX24 NUMMULTF.     NUMMULTX25 NUMMULTF.  
   NUMMULTX26 NUMMULTF.     NUMMULTX27 NUMMULTF.     NUMMULTX28 NUMMULTF.  
   NUMMULTX29 NUMMULTF.     NUMMULTX30 NUMMULTF.     NUMMULTX31 NUMMULTF.  
   NUMMULTX32 NUMMULTF.     NUMMULTX33 NUMMULTF.     NUMMULTX34 NUMMULTF.  
   NUMMULTX35 NUMMULTF.     NUMMULTX36 NUMMULTF.     NUMMULTX37 NUMMULTF.  
   NUMMULTX38 NUMMULTF.     NUMMULTX39 NUMMULTF.     NUMMULTX40 NUMMULTF.  
   NUMMULTX41 NUMMULTF.     NUMMULTX42 NUMMULTF.     NUMMULTX43 NUMMULTF.  
   NUMMULTX44 NUMMULTF.     NUMMULTX45 NUMMULTF.     NUMMULTX46 NUMMULTF.  
   NUMMULTX47 NUMMULTF.     NUMMULTX48 NUMMULTF.     SALMX1 N0Y1F.         
   SALMX2 N0Y1F.            SALMX3 N0Y1F.            SALMX4 N0Y1F.         
   SALMX5 N0Y1F.            SALMX6 N0Y1F.            SALMX7 N0Y1F.         
   SALMX8 N0Y1F.            SALMX9 N0Y1F.            SALMX10 N0Y1F.        
   SALMX11 N0Y1F.           SALMX12 N0Y1F.           SALMX13 N0Y1F.        
   SALMX14 N0Y1F.           SALMX15 N0Y1F.           SALMX16 N0Y1F.        
   SALMX17 N0Y1F.           SALMX18 N0Y1F.           SALMX19 N0Y1F.        
   SALMX20 N0Y1F.           SALMX21 N0Y1F.           SALMX22 N0Y1F.        
   SALMX23 N0Y1F.           SALMX24 N0Y1F.           SALMX25 N0Y1F.        
   SALMX26 N0Y1F.           SALMX27 N0Y1F.           SALMX28 N0Y1F.        
   SALMX29 N0Y1F.           SALMX30 N0Y1F.           SALMX31 N0Y1F.        
   SALMX32 N0Y1F.           SALMX33 N0Y1F.           SALMX34 N0Y1F.        
   SALMX35 N0Y1F.           SALMX36 N0Y1F.           SALMX37 N0Y1F.        
   SALMX38 N0Y1F.           SALMX39 N0Y1F.           SALMX40 N0Y1F.        
   SALMX41 N0Y1F.           SALMX42 N0Y1F.           SALMX43 N0Y1F.        
   SALMX44 N0Y1F.           SALMX45 N0Y1F.           SALMX46 N0Y1F.        
   SALMX47 N0Y1F.           SALMX48 N0Y1F.           SAYX1 N0Y1F.          
   SAYX2 N0Y1F.             SAYX3 N0Y1F.             SAYX4 N0Y1F.          
   SAYX5 N0Y1F.             SAYX6 N0Y1F.             SAYX7 N0Y1F.          
   SAYX8 N0Y1F.             SAYX9 N0Y1F.             SAYX10 N0Y1F.         
   SAYX11 N0Y1F.            SAYX12 N0Y1F.            SAYX13 N0Y1F.         
   SAYX14 N0Y1F.            SAYX15 N0Y1F.            SAYX16 N0Y1F.         
   SAYX17 N0Y1F.            SAYX18 N0Y1F.            SAYX19 N0Y1F.         
   SAYX20 N0Y1F.            SAYX21 N0Y1F.            SAYX22 N0Y1F.         
   SAYX23 N0Y1F.            SAYX24 N0Y1F.            SAYX25 N0Y1F.         
   SAYX26 N0Y1F.            SAYX27 N0Y1F.            SAYX28 N0Y1F.         
   SAYX29 N0Y1F.            SAYX30 N0Y1F.            SAYX31 N0Y1F.         
   SAYX32 N0Y1F.            SAYX33 N0Y1F.            SAYX34 N0Y1F.         
   SAYX35 N0Y1F.            SAYX36 N0Y1F.            SAYX37 N0Y1F.         
   SAYX38 N0Y1F.            SAYX39 N0Y1F.            SAYX40 N0Y1F.         
   SAYX41 N0Y1F.            SAYX42 N0Y1F.            SAYX43 N0Y1F.         
   SAYX44 N0Y1F.            SAYX45 N0Y1F.            SAYX46 N0Y1F.         
   SAYX47 N0Y1F.            SAYX48 N0Y1F.            SIMSEQX1 MC1SIMSQ.    
   SIMSEQX2 MC1SIMSQ.       SIMSEQX3 MC1SIMSQ.       SIMSEQX4 MC1SIMSQ.    
   SIMSEQX5 MC1SIMSQ.       SIMSEQX6 MC1SIMSQ.       SIMSEQX7 MC1SIMSQ.    
   SIMSEQX8 MC1SIMSQ.       SIMSEQX9 MC1SIMSQ.       SIMSEQX10 MC1SIMSQ.   
   SIMSEQX11 MC1SIMSQ.      SIMSEQX12 MC1SIMSQ.      SIMSEQX13 MC1SIMSQ.   
   SIMSEQX14 MC1SIMSQ.      SIMSEQX15 MC1SIMSQ.      SIMSEQX16 MC1SIMSQ.   
   SIMSEQX17 MC1SIMSQ.      SIMSEQX18 MC1SIMSQ.      SIMSEQX19 MC1SIMSQ.   
   SIMSEQX20 MC1SIMSQ.      SIMSEQX21 MC1SIMSQ.      SIMSEQX22 MC1SIMSQ.   
   SIMSEQX23 MC1SIMSQ.      SIMSEQX24 MC1SIMSQ.      SIMSEQX25 MC1SIMSQ.   
   SIMSEQX26 MC1SIMSQ.      SIMSEQX27 MC1SIMSQ.      SIMSEQX28 MC1SIMSQ.   
   SIMSEQX29 MC1SIMSQ.      SIMSEQX30 MC1SIMSQ.      SIMSEQX31 MC1SIMSQ.   
   SIMSEQX32 MC1SIMSQ.      SIMSEQX33 MC1SIMSQ.      SIMSEQX34 MC1SIMSQ.   
   SIMSEQX35 MC1SIMSQ.      SIMSEQX36 MC1SIMSQ.      SIMSEQX37 MC1SIMSQ.   
   SIMSEQX38 MC1SIMSQ.      SIMSEQX39 MC1SIMSQ.      SIMSEQX40 MC1SIMSQ.   
   SIMSEQX41 MC1SIMSQ.      SIMSEQX42 MC1SIMSQ.      SIMSEQX43 MC1SIMSQ.   
   SIMSEQX44 MC1SIMSQ.      SIMSEQX45 MC1SIMSQ.      SIMSEQX46 MC1SIMSQ.   
   SIMSEQX47 MC1SIMSQ.      SIMSEQX48 MC1SIMSQ.      USELSTP Y1N5RDF.      
   WYNOLSTP Y1N5RDF.        HPLSTP Y1N5RDF.          LSTMTHP1 FIRSMETHF.   
   LSTMTHP2 FIRSMETHF.      LSTMTHP3 FIRSMETHF.      LSTMTHP4 FIRSMETHF.   
   LPIUDTYP IUDTYPE.        USEFSTP Y1N5RDF.         FSTMTHP1 FIRSMETHF.   
   FSTMTHP2 FIRSMETHF.      FSTMTHP3 FIRSMETHF.      FSTMTHP4 FIRSMETHF.   
   USELSTP2 Y1N5RDF.        WYNOLSTP2 Y1N5RDF.       HPLSTP2 Y1N5RDF.      
   LSTMTHP5 FIRSMETHF.      LSTMTHP6 FIRSMETHF.      LSTMTHP7 FIRSMETHF.   
   LSTMTHP8 FIRSMETHF.      LPIUDTYP2 IUDTYPE.       USEFSTP2 Y1N5RDF.     
   FSTMTHP5 FIRSMETHF.      FSTMTHP6 FIRSMETHF.      FSTMTHP7 FIRSMETHF.   
   FSTMTHP8 FIRSMETHF.      USELSTP3 Y1N5RDF.        WYNOLSTP3 Y1N5RDF.    
   HPLSTP3 Y1N5RDF.         LSTMTHP9 FIRSMETHF.      LSTMTHP10 FIRSMETHF.  
   LSTMTHP11 FIRSMETHF.     LSTMTHP12 FIRSMETHF.     LPIUDTYP3 IUDTYPE.    
   USEFSTP3 Y1N5RDF.        FSTMTHP9 FIRSMETHF.      FSTMTHP10 FIRSMETHF.  
   FSTMTHP11 FIRSMETHF.     FSTMTHP12 FIRSMETHF.     WYNOTUSE Y1N5RDF.     
   HPPREGQ HPPREGQ.         DURTRY_N DURTRY_N.       DURTRY_P MNTHYRRD.    
   WHYNOUSING1 WHYNOUSNG.   WHYNOUSING2 WHYNOUSNG.   WHYNOUSING3 WHYNOUSNG.
   WHYNOUSING4 WHYNOUSNG.   WHYNOUSING5 WHYNOUSNG.   WHYNOTPG1 WHYNOTPGF.  
   WHYNOTPG2 WHYNOTPGF.     MAINNOUSE MAINNOUSE.     YUSEPILL1 YUSEMTHF.   
   YUSEPILL2 YUSEMTHF.      YUSEPILL3 YUSEMTHF.      YUSEPILL4 YUSEMTHF.   
   YUSEPILL5 YUSEMTHF.      YUSEPILL6 YUSEMTHF.      YUSEPILL7 YUSEMTHF.   
   IUDTYPE IUDTYPE.         YUSEIUD1 YUSEMTHF.       YUSEIUD2 YUSEMTHF.    
   YUSEIUD3 YUSEMTHF.       YUSEIUD4 YUSEMTHF.       YUSEIUD5 YUSEMTHF.    
   YUSEIUD6 YUSEMTHF.       EKMETHOD METHHXF.        CURBCPLC BCPLCF.      
   NUMPILLS NUMPILLS.       CURBCPAY1 PAYFMT.        CURBCPAY2 PAYFMT.     
   CURBCPAY3 PAYFMT.        CURBCINS Y1N5RDF.        NOUSEINS1 NOUSEINSF.  
   NOUSEINS2 NOUSEINSF.     NOUSEINS3 NOUSEINSF.     CURBCAMT CURBCAMT.    
   NOCOST1 Y1N5RDF.         NOCOST2 Y1N5RDF.         PST4WKSX PSTWKSF.     
   PSWKCOND1 Y1N5RDF.       PSWKCOND2 PSTWKSF.       COND1BRK Y1N5RDF.     
   COND1OFF Y1N5RDF.        CONDBRFL CONDTMS.        CONDOFF CONDTMS.      
   MISSPILL MISSPILL.       P12MOCON P12MOCON.       PXNOFREQ P12METHF.    
   BTHCON12 Y1N5RDF.        MEDTST12 Y1N5RDF.        BCCNS12 Y1N5RDF.      
   STEROP12 Y1N5RDF.        STCNS12 Y1N5RDF.         EMCON12 Y1N5RDF.      
   ECCNS12 Y1N5RDF.         NUMMTH12 NUMMTH12A.      PRGTST12 Y1N5RDF.     
   ABORT12 Y1N5RDF.         PAP12 Y1N5RDF.           PELVIC12 Y1N5RDF.     
   PRENAT12 Y1N5RDF.        PARTUM12 Y1N5RDF.        STDSVC12 Y1N5RDF.     
   BARRIER1 BARRIERF.       BARRIER2 BARRIERF.       BARRIER3 BARRIERF.    
   BARRIER4 BARRIERF.       BARRIER5 BARRIERF.       BARRIER6 BARRIERF.    
   NUMSVC12 NUMSVC12A.      NUMBCVIS NUMBCVIS.       BC12PLCX BCPLCF.      
   BC12PLCX2 BCPLCF.        BC12PLCX3 BCPLCF.        BC12PLCX4 BCPLCF.     
   BC12PLCX5 BCPLCF.        BC12PLCX6 BCPLCF.        BC12PLCX7 BCPLCF.     
   BC12PLCX8 BCPLCF.        BC12PLCX9 BCPLCF.        BC12PLCX10 BCPLCF.    
   BC12PLCX11 BCPLCF.       BC12PLCX12 BCPLCF.       BC12PLCX13 BCPLCF.    
   BC12PLCX14 BCPLCF.       BC12PLCX15 BCPLCF.       IDCLINIC IDCLINIC.    
   TALKPROV_1 TALKPROVF.    TALKPROV_2 TALKPROVF.    TALKPROV_3 TALKPROVF. 
   WHYPSTD WHYPSTDF.        WHYNOSTD WHYNOSTDF.      BCCLARC Y1N5RDF.      
   BC12PAYX1 PAYFMT.        BC12PAYX2 PAYFMT.        BC12PAYX3 PAYFMT.     
   BC12PAYX4 PAYFMT.        BC12PAYX7 PAYFMT.        BC12PAYX8 PAYFMT.     
   BC12PAYX9 PAYFMT.        BC12PAYX10 PAYFMT.       BC12PAYX13 PAYFMT.    
   BC12PAYX14 PAYFMT.       BC12PAYX15 PAYFMT.       BC12PAYX16 PAYFMT.    
   BC12PAYX19 PAYFMT.       BC12PAYX20 PAYFMT.       BC12PAYX21 PAYFMT.    
   BC12PAYX22 PAYFMT.       BC12PAYX25 PAYFMT.       BC12PAYX26 PAYFMT.    
   BC12PAYX27 PAYFMT.       BC12PAYX28 PAYFMT.       BC12PAYX31 PAYFMT.    
   BC12PAYX32 PAYFMT.       BC12PAYX33 PAYFMT.       BC12PAYX34 PAYFMT.    
   BC12PAYX37 PAYFMT.       BC12PAYX38 PAYFMT.       BC12PAYX39 PAYFMT.    
   BC12PAYX40 PAYFMT.       BC12PAYX43 PAYFMT.       BC12PAYX44 PAYFMT.    
   BC12PAYX45 PAYFMT.       BC12PAYX46 PAYFMT.       BC12PAYX49 PAYFMT.    
   BC12PAYX50 PAYFMT.       BC12PAYX51 PAYFMT.       BC12PAYX52 PAYFMT.    
   BC12PAYX55 PAYFMT.       BC12PAYX56 PAYFMT.       BC12PAYX57 PAYFMT.    
   BC12PAYX58 PAYFMT.       BC12PAYX61 PAYFMT.       BC12PAYX62 PAYFMT.    
   BC12PAYX63 PAYFMT.       BC12PAYX64 PAYFMT.       BC12PAYX67 PAYFMT.    
   BC12PAYX68 PAYFMT.       BC12PAYX69 PAYFMT.       BC12PAYX70 PAYFMT.    
   BC12PAYX73 PAYFMT.       BC12PAYX74 PAYFMT.       BC12PAYX75 PAYFMT.    
   BC12PAYX76 PAYFMT.       BC12PAYX79 PAYFMT.       BC12PAYX80 PAYFMT.    
   BC12PAYX81 PAYFMT.       BC12PAYX82 PAYFMT.       BC12PAYX85 PAYFMT.    
   BC12PAYX86 PAYFMT.       BC12PAYX87 PAYFMT.       BC12PAYX88 PAYFMT.    
   REGCAR12_F_01 REGCAR12F.   REGCAR12_F_02 REGCAR12F.   REGCAR12_F_03 REGCAR12F.
   REGCAR12_F_04 REGCAR12F.   REGCAR12_F_05 REGCAR12F.   REGCAR12_F_06 REGCAR12F.
   REGCAR12_F_07 REGCAR12F.   REGCAR12_F_08 REGCAR12F.   REGCAR12_F_09 REGCAR12F.
   REGCAR12_F_10 REGCAR12F.   REGCAR12_F_11 REGCAR12F.   REGCAR12_F_12 REGCAR12F.
   REGCAR12_F_13 REGCAR12F.   REGCAR12_F_14 REGCAR12F.   REGCAR12_F_15 REGCAR12F.
   EVERFPC Y1N5RDF.         KNDMDHLP01 KNDMDHLPF.    KNDMDHLP02 KNDMDHLPF. 
   KNDMDHLP03 KNDMDHLPF.    KNDMDHLP04 KNDMDHLPF.    KNDMDHLP05 KNDMDHLPF. 
   KNDMDHLP06 KNDMDHLPF.    KNDMDHLP07 KNDMDHLPF.    KNDMDHLP08 KNDMDHLPF. 
   KNDMDHLP09 KNDMDHLPF.    LASTPAP LASTPAP.         MREASPAP MREASFMT.    
   AGEFPAP AGEFIRST.        AGEFPAP2 AGEF2F.         ABNPAP3 ABNPAP3F.     
   INTPAP INTOFTF.          PELWPAP Y1N5RDF.         LASTPEL LASTPEL.      
   MREASPEL MREASFMT.       AGEFPEL AGEFIRST.        AGEPEL2 AGEF2F.       
   INTPEL INTOFTF.          EVHPVTST Y1N5RDF.        HPVWPAP Y1N5RDF.      
   LASTHPV LASTHPV.         MREASHPV MREASFMT.       AGEFHPV AGEFIRST.     
   AGEHPV2 AGEF2F.          INTHPV INTOFTF.          ASKSMOKE Y1N5RDF.     
   ASKPREG Y1N5RDF.         ASKFOLIC Y1N5RDF.        TALKDM Y1N5RDF.       
   PROVRESP PROVRATE.       PROVSAYBC PROVRATE.      PROVPREBC PROVRATE.   
   PROVINFOBC PROVRATE.     RWANT Y1N5RDF.           PROBWANT PROBWANT.    
   PWANT DEFPROBF.          JINTEND Y1N5RDF.         JSUREINT SUREINTCHP.  
   JINTENDN INTEXPF.        JEXPECTL INTEXPF.        JEXPECTS JEXPECTS.    
   JINTNEXT INTNEXT.        INTEND Y1N5RDF.          SUREINT SUREINTCHP.   
   INTENDN INTEXPF.         EXPECTL INTEXPF.         EXPECTS JEXPECTS.     
   INTNEXT INTNEXT.         HLPPRG Y1N5RDF.          HOWMANYR HOWMANYR.    
   SEEKWHO1 SEEKWHO1F.      SEEKWHO2 Y1N5RDF.        TYPALLPG1 TYPALLPGF.  
   TYPALLPG2 TYPALLPGF.     TYPALLPG3 TYPALLPGF.     TYPALLPG4 TYPALLPGF.  
   TYPALLPG5 TYPALLPGF.     TYPALLPG6 TYPALLPGF.     WHOTEST WHOTEST.      
   OVUL12M Y1N5RDF.         WHARTIN WHARTIN.         INSEM12M Y1N5RDF.     
   OTMEDHEP1 OTMEDHEPF.     OTMEDHEP2 OTMEDHEPF.     OTMEDHEP3 OTMEDHEPF.  
   OTMEDHEP4 OTMEDHEPF.     INSCOVPG Y1N5RDF.        FSTHLPPG_M MNTHFMT.   
   FSTHLPPG_Y YEARFMT.      CMPGVIS1 CMFMT.          TRYLONG2 TRYLONG2F.   
   UNIT_TRYLONG UNIT_TRYLONG.   HLPPGNOW Y1N5RDF.        RCNTPGH_M RCNTPGHF.   
   RCNTPGH_Y YEARFMT.       CMPGVISL CMPGVISL.       NUMVSTPG NUMVSTPG.    
   PRGVISIT PRGVISIT.       HLPMC Y1N5RDF.           TYPALLMC1 TYPALLMCF.  
   TYPALLMC2 TYPALLMCF.     TYPALLMC3 TYPALLMCF.     TYPALLMC4 TYPALLMCF.  
   TYPALLMC5 TYPALLMCF.     MISCNUM TPRGMISS.        INFRTPRB1 INFRTPRBF.  
   INFRTPRB2 INFRTPRBF.     INFRTPRB3 INFRTPRBF.     INFRTPRB4 INFRTPRBF.  
   INFRTPRB5 INFRTPRBF.     DUCHFREQ DUCHFREQ.       PID Y1N5RDF.          
   PIDSYMPT Y1N5RDF.        PIDTX PIDTX.             LSTPIDTX_M MNTHFMT.   
   LSTPIDTX_Y YEARFMT.      CMPIDLST CMFMT.          DIABETES DIABETES.    
   GESTDIAB Y1N5RDF.        UF Y1N5RDF.              UFSONO Y1N5RDF.       
   UFCURR Y1N5RDF.          UFDIAGNOS DHLONGF.       UFLIMIT Y1N5RDF.      
   UFTREAT_1 TREATF.        UFTREAT_2 TREATF.        UFTREAT_3 TREATF.     
   UFTREAT_4 TREATF.        UFTREAT_5 TREATF.        UFTREAT_6 TREATF.     
   UFTREAT_7 TREATF.        UFTREAT_8 TREATF.        ENDO Y1N5RDF.         
   ENDOCURR Y1N5RDF.        ENDODIAG DHLONGF.        ENDOLIM Y1N5RDF.      
   ENDOTREAT_1 TREATF.      ENDOTREAT_2 TREATF.      ENDOTREAT_3 TREATF.   
   ENDOTREAT_4 TREATF.      ENDOTREAT_5 TREATF.      ENDOTREAT_6 TREATF.   
   ENDOTREAT_7 TREATF.      ENDOTREAT_8 TREATF.      OVUPROB Y1N5RDF.      
   PCOS Y1N5RDF.            PCOSSYMP_1 PCOSSYMP.     PCOSSYMP_2 PCOSSYMP.  
   PCOSSYMP_3 PCOSSYMP.     PCOSSYMP_4 PCOSSYMP.     PCOSSYMP_5 PCOSSYMP.  
   PCOSSYMP_6 PCOSSYMP.     DEAF Y1N5RDF.            BLIND Y1N5RDF.        
   DIFDECIDE Y1N5RDF.       DIFWALK Y1N5RDF.         DIFDRESS Y1N5RDF.     
   DIFOUT Y1N5RDF.          EVRCANCER Y1N5RDF.       AGECANCER AGECANCER.  
   PRECANCER PRECANCER.     MAMMOG Y1N5RDF.          AGEMAMM1 AGEMAM1F.    
   REASMAMM1 REASMAMF.      CLINEXAM Y1N5RDF.        FAMHYST Y1N5RDF.      
   MOMRISK70 RISKF.         MOMRISK40 RISKF.         ALCORISK RISKF.       
   BFEEDRISK RISKF.         DONBLOOD Y1N5RDF.        HIVTEST Y1N5RDF.      
   NOHIVTST NOHIVTST.       WHENHIV_M MNTHFMT.       WHENHIV_Y YEARFMT.    
   CMHIVTST CMFMT.          HIVTSTYR Y1N5RDF.        HIVRESULT Y1N5RDF.    
   WHYNOGET WHYNOGET.       PLCHIV PLCHIV.           RHHIVT1 YESNONAF.     
   RHHIVT21 RHHIVT2F.       RHHIVT22 RHHIVT2F.       HIVTST HIVTST.        
   PREPHIV Y1N5RDF.         TALKDOCT Y1N5RDF.        AIDSTALK01 AIDSTALKF. 
   AIDSTALK02 AIDSTALKF.    AIDSTALK03 AIDSTALKF.    AIDSTALK04 AIDSTALKF. 
   AIDSTALK05 AIDSTALKF.    AIDSTALK06 AIDSTALKF.    AIDSTALK07 AIDSTALKF. 
   AIDSTALK08 AIDSTALKF.    AIDSTALK09 AIDSTALKF.    AIDSTALK10 AIDSTALKF. 
   AIDSTALK11 AIDSTALKF.    RETROVIR RETROVIR.       PREGHIV PREGHIV.      
   EVERVACC Y1N5RDF.        HPVSHOT1 AGE25NRDF.      HPVSEX1 HPVSEX1F.     
   VACCPROB LKNLK2FMT.      BLDPRESS Y1N5RDF.        HIGHBP HIGHBP.        
   BPMEDS Y1N5RDF.          NUTRINFO_1 NUTRINFO.     NUTRINFO_2 NUTRINFO.  
   NUTRINFO_3 NUTRINFO.     NUTRINFO_4 NUTRINFO.     NUTRINFO_5 NUTRINFO.  
   USUALCAR Y1N5RDF.        USLPLACE USLPLACE.       USL12MOS Y1N5RDF.     
   CURRCOV Y1N5RDF.         COVERHOW01 COVERHOWF.    COVERHOW02 COVERHOWF. 
   COVERHOW03 COVERHOWF.    PARINSUR Y1N5RDF.        INS_EXCH Y1N5RDF.     
   INS_PREM Y1N5RDF.        COVER12 Y1N5RDF.         NUMNOCOV NUMNOCOV.    
   SAMEADD Y1N5RDF.         BRNOUT Y1N5RDF.          RELRAISD RELCURR.     
   ATTND14 ATTNDF.          RELCURR RELCURR.         RELTRAD RELTRAD.      
   FUNDAM1 FUNDAMF.         FUNDAM2 FUNDAMF.         FUNDAM3 FUNDAMF.      
   FUNDAM4 FUNDAMF.         RELDLIFE RELDLIFE.       ATTNDNOW ATTNDF.      
   MILSVC Y1N5RDF.          WRK12MOS WRK12MOS.       FPT12MOS FPT12MOS.    
   DOLASTWK1 DOLASTWK.      DOLASTWK2 DOLASTWK.      DOLASTWK3 DOLASTWK.   
   DOLASTWK4 DOLASTWK.      DOLASTWK5 DOLASTWK.      DOLASTWK6 DOLASTWK.   
   RWRKST Y1N5C.            WORKP12 Y1N5C.           RPAYJOB Y1N5RDF.      
   RNUMJOB NUMJOBF.         RFTPTX TMWK12MO.         REARNTY Y1N5C.        
   SPLSTWK1 SPLSTWKF.       SPLSTWK2 SPLSTWKF.       SPLSTWK3 SPLSTWKF.    
   SPLSTWK4 SPLSTWKF.       SPLSTWK5 SPLSTWKF.       SPWRKST Y1N5C.        
   SPPAYJOB Y1N5RDF.        SPNUMJOB NUMJOBF.        SPFTPTX TMWK12MO.     
   SPEARNTY Y1N5C.          SAMESEX AGDGFMT.         CHSUPPOR AGDGFMT.     
   REACTSLF REACTSLF.       CHBOTHER CHBOTHER.       ACASILANG ACASILANG.  
   GENHEALT GENHEALT.       BMICAT BMICAT.           DRWEIGH YESNONAF.     
   TELLWGHT TELLWGHT.       WGHTSCRN YESNONAF.       ENGSPEAK ENGSPEAK.    
   CASIBIRTH CASINUM.       CASILOSS CASINUM.        CASIABOR CASINUM.     
   CASIADOP YESNONAF.       NOBEDYR YESNONAF.        STAYREL YESNONAF.     
   EVSUSPEN YESNONAF.       GRADSUSP CASIGRAD.       SMK100 YESNONAF.      
   AGESMK CASISMK.          SMOKE12 SMOKE12F.        SMKSTOP YESNONAF.     
   DRINK12 DRINKF.          UNIT30D UNIT30D.         DRINK30D DRNK30D.     
   DRINKDAY DRNKNUM.        BINGE30 BNGE30T.         DRNKMOST DRNKNUM.     
   BINGE12 DRINKF.          POT12 POTFRF.            COC12 DRUGFRF.        
   CRACK12 DRUGFRF.         CRYSTMTH12 DRUGFRF.      INJECT12 DRUGFRF.     
   VAGSEX YESNONAF.         AGEVAGR AGEVAGR.         CONDVAG YESNONAF.     
   WHYCONDL WHYCONDL.       GETORALM YESNONAF.       GIVORALM YESNONAF.    
   CONDFELL YESNONAF.       ANYORAL Y1N5NAC.         TIMING TIMING.        
   ANALSEX YESNONAF.        CONDANAL YESNONAF.       OPPSEXANY Y1N5NAC.    
   OPPSEXGEN Y1N5NAC.       CONDSEXL YESNONAF.       OPPLIFENUM PARTLIF.   
   OPPYEARNUM PRTS12MB.     VAGNUM12 PRTS12MB.       ORALNUM12 PRTS12MB.   
   ANALNUM12 PRTS12MB.      BISEXPRT YESNONAF.       NONMONOG YESNONAF.    
   NNONMONOG1 NNONMONOG1F.   NNONMONOG2 NNONMONOG2F.   NNONMONOG3 NNONMONOG2F.
   MALSHT12 YESNONAF.       PROSTFRQ YESNONAF.       JOHNFREQ YESNONAF.    
   HIVMAL12 YESNONAF.       GIVORALF YESNONAF.       GETORALF YESNONAF.    
   FEMSEX YESNONAF.         SAMESEXANY Y1N5NAC.      SAMLIFENUM PRTLIFEF.  
   SAMYEARNUM PRT12F.       SAMESEX1 SAMESEX1F.      FSAMEREL FSAMEREL.    
   MFLASTP MALFEMNAF.       DATEAPP Y1N5RDF.         ATTRACT ATTRACT.      
   ORIENT_A ORIENT_A.       ORIENT_B ORIENT_B.       CONFCONC YESNONAF.    
   TIMALON TIMALON.         RISKCHEK1 YESNONAF.      RISKCHEK2 YESNONAF.   
   RISKCHEK3 YESNONAF.      RISKCHEK4 YESNONAF.      CHLAMTST YESNONAF.    
   STDOTHR12 YESNONAF.      STDTRT12 YESNONAF.       GON YESNONAF.         
   CHLAM YESNONAF.          HERPES YESNONAF.         GENWARTS YESNONAF.    
   SYPHILIS YESNONAF.       EVRINJECT YESNONAF.      EVRSHARE YESNONAF.    
   EARNTYPE INCWMYF.        EARN EARN.               EARNDK1 YESNONAF.     
   EARNDK2 YESNONAF.        EARNDK3 YESNONAF.        EARNDK4 YESNONAF.     
   TOINCWMY INCWMYF.        TOTINC EARN.             FMINCDK1 FMINCDK1F.   
   FMINCDK2 YESNONAF.       FMINCDK3 YESNONAF.       FMINCDK4 YESNONAF.    
   FMINCDK5 YESNONAF.       PUBASST YESNONAF.        PUBASTYP1 P_ASTYP.    
   FOODSTMP YESNONAF.       WIC YESNONAF.            HLPTRANS YESNONAF.    
   HLPCHLDC YESNONAF.       HLPJOB YESNONAF.         FREEFOOD YESNONAF.    
   HUNGRY YESNONAF.         MED_COST YESNONAF.       AGER AGERFF.          
   FMARITAL FMARITAL.       RMARITAL RMARITAL.       EDUCAT EDUCAT.        
   HIEDUC HIEDUC.           HISPANIC HISPANIC.       RACE RACE.            
   HISPRACE2 HISPRACE2F.    NUMKDHH NUMKDHH.         NUMFMHH NUMFMHH.      
   HHFAMTYP HHFAMTYP.       HHPARTYP HHPARTYP.       NCHILDHH NCHILDHH.    
   HHKIDTYP HHKIDTYP.       CSPBBHH CSPBIO.          CSPBSHH CSPNOT.       
   CSPOKDHH CSPNOT.         INTCTFAM INTCTFAM.       PARAGE14 PARAGEF.     
   EDUCMOM EDUCMOM.         AGEMOMB1 AGEMOMBF.       AGER_I IMPFLG.        
   FMARITAL_I IMPFLG.       RMARITAL_I IMPFLG.       EDUCAT_I IMPFLG.      
   HIEDUC_I IMPFLG.         HISPANIC_I IMPFLG.       RACE_I IMPFLG.        
   HISPRACE2_I IMPFLG.      NUMKDHH_I IMPFLG.        NUMFMHH_I IMPFLG.     
   HHFAMTYP_I IMPFLG.       HHPARTYP_I IMPFLG.       NCHILDHH_I IMPFLG.    
   HHKIDTYP_I IMPFLG.       CSPBBHH_I IMPFLG.        CSPBSHH_I IMPFLG.     
   CSPOKDHH_I IMPFLG.       INTCTFAM_I IMPFLG.       PARAGE14_I IMPFLG.    
   EDUCMOM_I IMPFLG.        AGEMOMB1_I IMPFLG.       RCURPREG RCURPREG.    
   PREGNUM PREGNUMF.        COMPREG PREGNUMF.        LOSSNUM TPRGOUTF.     
   ABORTION TPRGOUTF.       LBPREGS LBPREGS.         PARITY PARITY.        
   BIRTHS5 BIRTHS5F.        BIRTH1 Y1N2RECF.         OUTCOM01 OUTCOMRF.    
   OUTCOM02 OUTCOMRF.       OUTCOM03 OUTCOMRF.       OUTCOM04 OUTCOMRF.    
   OUTCOM05 OUTCOMRF.       OUTCOM06 OUTCOMRF.       OUTCOM07 OUTCOMRF.    
   OUTCOM08 OUTCOMRF.       OUTCOM09 OUTCOMRF.       OUTCOM10 OUTCOMRF.    
   OUTCOM11 OUTCOMRF.       OUTCOM12 OUTCOMRF.       OUTCOM13 OUTCOMRF.    
   OUTCOM14 OUTCOMRF.       DATEND01 YEARFMT.        DATEND02 YEARFMT.     
   DATEND03 YEARFMT.        DATEND04 YEARFMT.        DATEND05 YEARFMT.     
   DATEND06 YEARFMT.        DATEND07 YEARFMT.        DATEND08 YEARFMT.     
   DATEND09 YEARFMT.        DATEND10 YEARFMT.        DATEND11 YEARFMT.     
   DATEND12 YEARFMT.        DATEND13 YEARFMT.        DATEND14 YEARFMT.     
   DATCON01 YEARFMT.        DATCON02 YEARFMT.        DATCON03 YEARFMT.     
   DATCON04 YEARFMT.        DATCON05 YEARFMT.        DATCON06 YEARFMT.     
   DATCON07 YEARFMT.        DATCON08 YEARFMT.        DATCON09 YEARFMT.     
   DATCON10 YEARFMT.        DATCON11 YEARFMT.        DATCON12 YEARFMT.     
   DATCON13 YEARFMT.        DATCON14 YEARFMT.        MAROUT01 MARPRGF.     
   MAROUT02 MARPRGF.        MAROUT03 MARPRGF.        MAROUT04 MARPRGF.     
   MAROUT05 MARPRGF.        MAROUT06 MARPRGF.        MAROUT07 MARPRGF.     
   MAROUT08 MARPRGF.        MAROUT09 MARPRGF.        MAROUT10 MARPRGF.     
   MAROUT11 MARPRGF.        MAROUT12 MARPRGF.        MAROUT13 MARPRGF.     
   MAROUT14 MARPRGF.        RMAROUT01 RMARFMT.       RMAROUT02 RMARFMT.    
   RMAROUT03 RMARFMT.       RMAROUT04 RMARFMT.       RMAROUT05 RMARFMT.    
   RMAROUT06 RMARFMT.       RMAROUT07 RMARFMT.       RMAROUT08 RMARFMT.    
   RMAROUT09 RMARFMT.       RMAROUT10 RMARFMT.       RMAROUT11 RMARFMT.    
   RMAROUT12 RMARFMT.       RMAROUT13 RMARFMT.       RMAROUT14 RMARFMT.    
   MARCON01 MARPRGF.        MARCON02 MARPRGF.        MARCON03 MARPRGF.     
   MARCON04 MARPRGF.        MARCON05 MARPRGF.        MARCON06 MARPRGF.     
   MARCON07 MARPRGF.        MARCON08 MARPRGF.        MARCON09 MARPRGF.     
   MARCON10 MARPRGF.        MARCON11 MARPRGF.        MARCON12 MARPRGF.     
   MARCON13 MARPRGF.        MARCON14 MARPRGF.        RMARCON01 RMARFMT.    
   RMARCON02 RMARFMT.       RMARCON03 RMARFMT.       RMARCON04 RMARFMT.    
   RMARCON05 RMARFMT.       RMARCON06 RMARFMT.       RMARCON07 RMARFMT.    
   RMARCON08 RMARFMT.       RMARCON09 RMARFMT.       RMARCON10 RMARFMT.    
   RMARCON11 RMARFMT.       RMARCON12 RMARFMT.       RMARCON13 RMARFMT.    
   RMARCON14 RMARFMT.       CEBOW CEBOW.             CEBOWC CEBOW.         
   DATBABY1 YEARFMT.        LASTPREG LASTPREG.       PREG_INT1 PREGINT.    
   PREG_INT2 PREGINT.       PREG_INT3 PREGINT.       PREG_INT4 PREGINT.    
   RCURPREG_I IMPFLG.       PREGNUM_I IMPFLG.        COMPREG_I IMPFLG.     
   LOSSNUM_I IMPFLG.        ABORTION_I IMPFLG.       LBPREGS_I IMPFLG.     
   PARITY_I IMPFLG.         BIRTHS5_I IMPFLG.        OUTCOM01_I IMPFLG.    
   OUTCOM02_I IMPFLG.       OUTCOM03_I IMPFLG.       OUTCOM04_I IMPFLG.    
   OUTCOM05_I IMPFLG.       OUTCOM06_I IMPFLG.       OUTCOM07_I IMPFLG.    
   OUTCOM08_I IMPFLG.       OUTCOM09_I IMPFLG.       OUTCOM10_I IMPFLG.    
   OUTCOM11_I IMPFLG.       OUTCOM12_I IMPFLG.       OUTCOM13_I IMPFLG.    
   OUTCOM14_I IMPFLG.       DATEND01_I IMPFLG.       DATEND02_I IMPFLG.    
   DATEND03_I IMPFLG.       DATEND04_I IMPFLG.       DATEND05_I IMPFLG.    
   DATEND06_I IMPFLG.       DATEND07_I IMPFLG.       DATEND08_I IMPFLG.    
   DATEND09_I IMPFLG.       DATEND10_I IMPFLG.       DATEND11_I IMPFLG.    
   DATEND12_I IMPFLG.       DATEND13_I IMPFLG.       DATEND14_I IMPFLG.    
   AGEPRG01_I IMPFLG.       AGEPRG02_I IMPFLG.       AGEPRG03_I IMPFLG.    
   AGEPRG04_I IMPFLG.       AGEPRG05_I IMPFLG.       AGEPRG06_I IMPFLG.    
   AGEPRG07_I IMPFLG.       AGEPRG08_I IMPFLG.       AGEPRG09_I IMPFLG.    
   AGEPRG10_I IMPFLG.       AGEPRG11_I IMPFLG.       AGEPRG12_I IMPFLG.    
   AGEPRG13_I IMPFLG.       AGEPRG14_I IMPFLG.       DATCON01_I IMPFLG.    
   DATCON02_I IMPFLG.       DATCON03_I IMPFLG.       DATCON04_I IMPFLG.    
   DATCON05_I IMPFLG.       DATCON06_I IMPFLG.       DATCON07_I IMPFLG.    
   DATCON08_I IMPFLG.       DATCON09_I IMPFLG.       DATCON10_I IMPFLG.    
   DATCON11_I IMPFLG.       DATCON12_I IMPFLG.       DATCON13_I IMPFLG.    
   DATCON14_I IMPFLG.       AGECON01_I IMPFLG.       AGECON02_I IMPFLG.    
   AGECON03_I IMPFLG.       AGECON04_I IMPFLG.       AGECON05_I IMPFLG.    
   AGECON06_I IMPFLG.       AGECON07_I IMPFLG.       AGECON08_I IMPFLG.    
   AGECON09_I IMPFLG.       AGECON10_I IMPFLG.       AGECON11_I IMPFLG.    
   AGECON12_I IMPFLG.       AGECON13_I IMPFLG.       AGECON14_I IMPFLG.    
   MAROUT01_I IMPFLG.       MAROUT02_I IMPFLG.       MAROUT03_I IMPFLG.    
   MAROUT04_I IMPFLG.       MAROUT05_I IMPFLG.       MAROUT06_I IMPFLG.    
   MAROUT07_I IMPFLG.       MAROUT08_I IMPFLG.       MAROUT09_I IMPFLG.    
   MAROUT10_I IMPFLG.       MAROUT11_I IMPFLG.       MAROUT12_I IMPFLG.    
   MAROUT13_I IMPFLG.       MAROUT14_I IMPFLG.       RMAROUT01_I IMPFLG.   
   RMAROUT02_I IMPFLG.      RMAROUT03_I IMPFLG.      RMAROUT04_I IMPFLG.   
   RMAROUT05_I IMPFLG.      RMAROUT06_I IMPFLG.      RMAROUT07_I IMPFLG.   
   RMAROUT08_I IMPFLG.      RMAROUT09_I IMPFLG.      RMAROUT10_I IMPFLG.   
   RMAROUT11_I IMPFLG.      RMAROUT12_I IMPFLG.      RMAROUT13_I IMPFLG.   
   RMAROUT14_I IMPFLG.      MARCON01_I IMPFLG.       MARCON02_I IMPFLG.    
   MARCON03_I IMPFLG.       MARCON04_I IMPFLG.       MARCON05_I IMPFLG.    
   MARCON06_I IMPFLG.       MARCON07_I IMPFLG.       MARCON08_I IMPFLG.    
   MARCON09_I IMPFLG.       MARCON10_I IMPFLG.       MARCON11_I IMPFLG.    
   MARCON12_I IMPFLG.       MARCON13_I IMPFLG.       MARCON14_I IMPFLG.    
   RMARCON01_I IMPFLG.      RMARCON02_I IMPFLG.      RMARCON03_I IMPFLG.   
   RMARCON04_I IMPFLG.      RMARCON05_I IMPFLG.      RMARCON06_I IMPFLG.   
   RMARCON07_I IMPFLG.      RMARCON08_I IMPFLG.      RMARCON09_I IMPFLG.   
   RMARCON10_I IMPFLG.      RMARCON11_I IMPFLG.      RMARCON12_I IMPFLG.   
   RMARCON13_I IMPFLG.      RMARCON14_I IMPFLG.      CEBOW_I IMPFLG.       
   CEBOWC_I IMPFLG.         DATBABY1_I IMPFLG.       AGEBABY1_I IMPFLG.    
   FMARNO FMARNO.           MARDAT01 YEARFMT.        MARDAT02 YEARFMT.     
   MARDAT03 YEARFMT.        MARDAT04 YEARFMT.        MARDIS01 YEARFMT.     
   MARDIS02 YEARFMT.        MARDIS03 YEARFMT.        MARDIS04 YEARFMT.     
   MAREND01 MARENDF.        MAREND02 MARENDF.        MAREND03 MARENDF.     
   MAREND04 MARENDF.        MAR1DISS MARCATA.        DD1REMAR MARCATA.     
   MAR1BIR1 MARCATB.        MAR1CON1 MARCATC.        CON1MAR1 MARCATD.     
   B1PREMAR Y1N2RECF.       COHEVER COHEVER.         EVMARCOH EVMARCOH.    
   PMARRNO COHNUM2F.        NONMARR COHNUM2F.        TIMESCOH COHNUM2F.    
   COHAB1 YEARFMT.          COHSTAT COHSTAT.         COHOUT COHOUT.        
   COH1DUR MARCATA.         HADSEX HADSEX.           SEXONCE SEXONCE.      
   SEXEVER SEXEVER.         VRY1STAG SEX1RECF.       SEX1AGE SEX1RECF.     
   VRY1STSX VRY1STSX.       DATESEX1 DATESEX1F.      SEXMAR MARCATE.       
   SEX1FOR MARCATF.         SEXUNION MARCATG.        SEXOUT SEXOUT.        
   FPDUR FPDUR.             PARTS1YR PARTS1YR.       LSEXDATE CMFMT.       
   LSEXPREG Y1N2RECF.       SEX3MO Y1N2RECF.         NUMP3MOS NUMP3MOS.    
   LSEXRAGE LSEXRAGE.       PARTDUR1 PARTDURF.       PARTDUR2 PARTDURF.    
   PARTDUR3 PARTDURF.       RELATP1 RELATPF.         RELATP2 RELATPF.      
   RELATP3 RELATPF.         LIFPRTNR LIFPRTNR.       FMARNO_I IMPFLG.      
   CSPBIOKD_I IMPFLG.       MARDAT01_I IMPFLG.       MARDAT02_I IMPFLG.    
   MARDAT03_I IMPFLG.       MARDAT04_I IMPFLG.       MARDIS01_I IMPFLG.    
   MARDIS02_I IMPFLG.       MARDIS03_I IMPFLG.       MARDIS04_I IMPFLG.    
   MAREND01_I IMPFLG.       MAREND02_I IMPFLG.       MAREND03_I IMPFLG.    
   MAREND04_I IMPFLG.       FMAR1AGE_I IMPFLG.       AGEDISS1_I IMPFLG.    
   AGEDD1_I IMPFLG.         MAR1DISS_I IMPFLG.       DD1REMAR_I IMPFLG.    
   MAR1BIR1_I IMPFLG.       MAR1CON1_I IMPFLG.       CON1MAR1_I IMPFLG.    
   B1PREMAR_I IMPFLG.       COHEVER_I IMPFLG.        EVMARCOH_I IMPFLG.    
   PMARRNO_I IMPFLG.        NONMARR_I IMPFLG.        TIMESCOH_I IMPFLG.    
   COHAB1_I IMPFLG.         COHSTAT_I IMPFLG.        COHOUT_I IMPFLG.      
   COH1DUR_I IMPFLG.        HADSEX_I IMPFLG.         SEXEVER_I IMPFLG.     
   VRY1STAG_I IMPFLG.       SEX1AGE_I IMPFLG.        VRY1STSX_I IMPFLG.    
   DATESEX1_I IMPFLG.       SEXONCE_I IMPFLG.        SEXMAR_I IMPFLG.      
   SEX1FOR_I IMPFLG.        SEXUNION_I IMPFLG.       SEXOUT_I IMPFLG.      
   FPDUR_I IMPFLG.          PARTS1YR_I IMPFLG.       LSEXDATE_I IMPFLG.    
   SEXP3MO_I IMPFLG.        NUMP3MOS_I IMPFLG.       LSEXRAGE_I IMPFLG.    
   PARTDUR1_I IMPFLG.       PARTDUR2_I IMPFLG.       PARTDUR3_I IMPFLG.    
   RELATP1_I IMPFLG.        RELATP2_I IMPFLG.        RELATP3_I IMPFLG.     
   LIFPRTNR_I IMPFLG.       STRLOPER STRLOPER.       FECUND FECUND.        
   ANYBC36 ANYBCF.          NOSEX36 NOSEX36F.        INFERT INFERT.        
   ANYBC12 ANYBCF.          ANYMTHD Y1N2RECF.        NOSEX12 NOSEX12F.     
   SEXP3MO Y1N2RECF.        CONSTAT1 CONSTATF.       CONSTAT2 CONSTATF.    
   CONSTAT3 CONSTATF.       CONSTAT4 CONSTATF.       PILLR Y1N2RECF.       
   CONDOMR Y1N2RECF.        SEX1MTHD1 SEX1MTHDF.     SEX1MTHD2 SEX1MTHDF.  
   SEX1MTHD3 SEX1MTHDF.     SEX1MTHD4 SEX1MTHDF.     MTHUSE12 MTHUSE12F.   
   METH12M1 METH12MF.       METH12M2 METH12MF.       METH12M3 METH12MF.    
   METH12M4 METH12MF.       MTHUSE3 MTHUSE3F.        METH3M1 METH3MF.      
   METH3M2 METH3MF.         METH3M3 METH3MF.         METH3M4 METH3MF.      
   FMETHOD1 FMETHODF.       FMETHOD2 FMETHODF.       FMETHOD3 FMETHODF.    
   FMETHOD4 FMETHODF.       OLDWP01 OWWNTF.          OLDWP02 OWWNTF.       
   OLDWP03 OWWNTF.          OLDWP04 OWWNTF.          OLDWP05 OWWNTF.       
   OLDWP06 OWWNTF.          OLDWP07 OWWNTF.          OLDWP08 OWWNTF.       
   OLDWP09 OWWNTF.          OLDWP10 OWWNTF.          OLDWP11 OWWNTF.       
   OLDWP12 OWWNTF.          OLDWP13 OWWNTF.          OLDWP14 OWWNTF.       
   OLDWR01 OWWNTF.          OLDWR02 OWWNTF.          OLDWR03 OWWNTF.       
   OLDWR04 OWWNTF.          OLDWR05 OWWNTF.          OLDWR06 OWWNTF.       
   OLDWR07 OWWNTF.          OLDWR08 OWWNTF.          OLDWR09 OWWNTF.       
   OLDWR10 OWWNTF.          OLDWR11 OWWNTF.          OLDWR12 OWWNTF.       
   OLDWR13 OWWNTF.          OLDWR14 OWWNTF.          WANTRP01 OWWNTF.      
   WANTRP02 OWWNTF.         WANTRP03 OWWNTF.         WANTRP04 OWWNTF.      
   WANTRP05 OWWNTF.         WANTRP06 OWWNTF.         WANTRP07 OWWNTF.      
   WANTRP08 OWWNTF.         WANTRP09 OWWNTF.         WANTRP10 OWWNTF.      
   WANTRP11 OWWNTF.         WANTRP12 OWWNTF.         WANTRP13 OWWNTF.      
   WANTRP14 OWWNTF.         WANTP01 OWWNTF.          WANTP02 OWWNTF.       
   WANTP03 OWWNTF.          WANTP04 OWWNTF.          WANTP05 OWWNTF.       
   WANTP06 OWWNTF.          WANTP07 OWWNTF.          WANTP08 OWWNTF.       
   WANTP09 OWWNTF.          WANTP10 OWWNTF.          WANTP11 OWWNTF.       
   WANTP12 OWWNTF.          WANTP13 OWWNTF.          WANTP14 OWWNTF.       
   NWWANTRP01 NWWANTRPF.    NWWANTRP02 NWWANTRPF.    NWWANTRP03 NWWANTRPF. 
   NWWANTRP04 NWWANTRPF.    NWWANTRP05 NWWANTRPF.    NWWANTRP06 NWWANTRPF. 
   NWWANTRP07 NWWANTRPF.    NWWANTRP08 NWWANTRPF.    NWWANTRP09 NWWANTRPF. 
   NWWANTRP10 NWWANTRPF.    NWWANTRP11 NWWANTRPF.    NWWANTRP12 NWWANTRPF. 
   NWWANTRP13 NWWANTRPF.    NWWANTRP14 NWWANTRPF.    WANTP5 WANTP5F.       
   STRLOPER_I IMPFLG.       FECUND_I IMPFLG.         INFERT_I IMPFLG.      
   ANYMTHD_I IMPFLG.        NOSEX12_I IMPFLG.        SEX3MO_I IMPFLG.      
   CONSTAT1_I IMPFLG.       CONSTAT2_I IMPFLG.       CONSTAT3_I IMPFLG.    
   CONSTAT4_I IMPFLG.       PILLR_I IMPFLG.          CONDOMR_I IMPFLG.     
   SEX1MTHD1_I IMPFLG.      SEX1MTHD2_I IMPFLG.      SEX1MTHD3_I IMPFLG.   
   SEX1MTHD4_I IMPFLG.      MTHUSE12_I IMPFLG.       METH12M1_I IMPFLG.    
   METH12M2_I IMPFLG.       METH12M3_I IMPFLG.       METH12M4_I IMPFLG.    
   MTHUSE3_I IMPFLG.        METH3M1_I IMPFLG.        METH3M2_I IMPFLG.     
   METH3M3_I IMPFLG.        METH3M4_I IMPFLG.        FMETHOD1_I IMPFLG.    
   FMETHOD2_I IMPFLG.       FMETHOD3_I IMPFLG.       FMETHOD4_I IMPFLG.    
   DATEUSE1_I IMPFLG.       OLDWP01_I IMPFLG.        OLDWP02_I IMPFLG.     
   OLDWP03_I IMPFLG.        OLDWP04_I IMPFLG.        OLDWP05_I IMPFLG.     
   OLDWP06_I IMPFLG.        OLDWP07_I IMPFLG.        OLDWP08_I IMPFLG.     
   OLDWP09_I IMPFLG.        OLDWP10_I IMPFLG.        OLDWP11_I IMPFLG.     
   OLDWP12_I IMPFLG.        OLDWP13_I IMPFLG.        OLDWP14_I IMPFLG.     
   OLDWR01_I IMPFLG.        OLDWR02_I IMPFLG.        OLDWR03_I IMPFLG.     
   OLDWR04_I IMPFLG.        OLDWR05_I IMPFLG.        OLDWR06_I IMPFLG.     
   OLDWR07_I IMPFLG.        OLDWR08_I IMPFLG.        OLDWR09_I IMPFLG.     
   OLDWR10_I IMPFLG.        OLDWR11_I IMPFLG.        OLDWR12_I IMPFLG.     
   OLDWR13_I IMPFLG.        OLDWR14_I IMPFLG.        WANTRP01_I IMPFLG.    
   WANTRP02_I IMPFLG.       WANTRP03_I IMPFLG.       WANTRP04_I IMPFLG.    
   WANTRP05_I IMPFLG.       WANTRP06_I IMPFLG.       WANTRP07_I IMPFLG.    
   WANTRP08_I IMPFLG.       WANTRP09_I IMPFLG.       WANTRP10_I IMPFLG.    
   WANTRP11_I IMPFLG.       WANTRP12_I IMPFLG.       WANTRP13_I IMPFLG.    
   WANTRP14_I IMPFLG.       WANTP01_I IMPFLG.        WANTP02_I IMPFLG.     
   WANTP03_I IMPFLG.        WANTP04_I IMPFLG.        WANTP05_I IMPFLG.     
   WANTP06_I IMPFLG.        WANTP07_I IMPFLG.        WANTP08_I IMPFLG.     
   WANTP09_I IMPFLG.        WANTP10_I IMPFLG.        WANTP11_I IMPFLG.     
   WANTP12_I IMPFLG.        WANTP13_I IMPFLG.        WANTP14_I IMPFLG.     
   NWWANTRP01_I IMPFLG.     NWWANTRP02_I IMPFLG.     NWWANTRP03_I IMPFLG.  
   NWWANTRP04_I IMPFLG.     NWWANTRP05_I IMPFLG.     NWWANTRP06_I IMPFLG.  
   NWWANTRP07_I IMPFLG.     NWWANTRP08_I IMPFLG.     NWWANTRP09_I IMPFLG.  
   NWWANTRP10_I IMPFLG.     NWWANTRP11_I IMPFLG.     NWWANTRP12_I IMPFLG.  
   NWWANTRP13_I IMPFLG.     NWWANTRP14_I IMPFLG.     WANTP5_I IMPFLG.      
   FPTIT12 FPTIT.           FPTITMED FPTIT.          FPTITSTE SRCSRV.      
   FPTITBC SRCSRV.          FPTITCHK SRCSRV.         FPTITCBC SRCSRV.      
   FPTITCST SRCSRV.         FPTITEC SRCSRV.          FPTITCEC SRCSRV.      
   FPTITPRE SRCSRV.         FPTITABO SRCSRV.         FPTITPAP SRCSRV.      
   FPTITPEL SRCSRV.         FPTITPRN SRCSRV.         FPTITPPR SRCSRV.      
   FPTITSTD SRCSRV.         FPREGFP Y1N2RECF.        FPREGMED Y1N2RECF.    
   FPTIT12_I IMPFLG.        FPTITMED_I IMPFLG.       FPTITSTE_I IMPFLG.    
   FPTITBC_I IMPFLG.        FPTITCHK_I IMPFLG.       FPTITCBC_I IMPFLG.    
   FPTITCST_I IMPFLG.       FPTITEC_I IMPFLG.        FPTITCEC_I IMPFLG.    
   FPTITPRE_I IMPFLG.       FPTITABO_I IMPFLG.       FPTITPAP_I IMPFLG.    
   FPTITPEL_I IMPFLG.       FPTITPRN_I IMPFLG.       FPTITPPR_I IMPFLG.    
   FPTITSTD_I IMPFLG.       FPREGFP_I IMPFLG.        FPREGMED_I IMPFLG.    
   INTENT INTENT.           ADDEXP ADDEXP.           INTENT_I IMPFLG.      
   ADDEXP_I IMPFLG.         ANYPRGHP Y1N2RECF.       ANYMSCHP Y1N2RECF.    
   INFEVER Y1N2RECF.        OVULATE REPORTF.         TUBES REPORTF.        
   INFERTR REPORTF.         INFERTH REPORTF.         ADVICE REPORTF.       
   INSEM REPORTF.           INVITRO REPORTF.         ENDOMET REPORTF.      
   FIBROIDS REPORTF.        PIDTREAT Y1N2RECF.       EVHIVTST EVHIVTST.    
   FPTITHIV FPTITHIV.       ANYPRGHP_I IMPFLG.       ANYMSCHP_I IMPFLG.    
   INFEVER_I IMPFLG.        OVULATE_I IMPFLG.        TUBES_I IMPFLG.       
   INFERTR_I IMPFLG.        INFERTH_I IMPFLG.        ADVICE_I IMPFLG.      
   INSEM_I IMPFLG.          INVITRO_I IMPFLG.        ENDOMET_I IMPFLG.     
   FIBROIDS_I IMPFLG.       PIDTREAT_I IMPFLG.       EVHIVTST_I IMPFLG.    
   FPTITHIV_I IMPFLG.       CURR_INS CURR_INS.       METRO METRO.          
   RELIGION RELIGION.       LABORFOR LABORFOR.       CURR_INS_I IMPFLG.    
   RELIGION_I IMPFLG.       LABORFOR_I IMPFLG.       POVERTY POVERTY.      
   TOTINCR TOTINCR.         PUBASSIS PUBASSIS.       POVERTY_I IMPFLG.     
   TOTINCR_I IMPFLG.        PUBASSIS_I IMPFLG.       SECU SECU.            
   CMINTVW CMFMT.           CMLSTYR CMFMT.           CMJAN3YR CMJAN3YRF.   
   CMJAN4YR CMJAN4YRF.      CMJAN5YR CMJAN5YRF.            
   PHASE PHASE. ;
*/

* SAS LENGTH STATEMENT;

LENGTH
   CASEID 6                 RSCRNINF 3               RSCRAGE 3             
   RSCRHISP 3               RSCRRACE 3               AGE_A 3               
   AGE_R 3                  AGESCRN 3                HISP 3                
   HISPGRP 3                PRIMLANG1 3              PRIMLANG2 3           
   PRIMLANG3 3              ROSCNT 3                 NUMCHILD 3            
   HHKIDS18 3               DAUGHT918 3              SON918 3              
   NONBIOKIDS 3             MARSTAT 3                FMARSTAT 3            
   FMARIT 3                 EVRMARRY 3               HPLOCALE 3            
   MANREL 3                 GOSCHOL 3                VACA 3                
   HIGRADE 3                COMPGRD 3                DIPGED 3              
   EARNHS_Y 4               HISCHGRD 3               LSTGRADE 3            
   MYSCHOL_Y 4              HAVEDEG 3                DEGREES 3             
   EARNBA_Y 4               EXPSCHL 3                EXPGRADE 3            
   WTHPARNW 3               ONOWN 3                  ONOWN18 3             
   INTACT 3                 PARMARR 3                INTACT18 3            
   LVSIT14F 3               LVSIT14M 3               WOMRASDU 3            
   MOMDEGRE 3               MOMWORKD 3               MOMFSTCH 3            
   MOM18 3                  MANRASDU 3               FOSTEREV 3            
   MNYFSTER 3               DURFSTER 3               AGEFSTER 3            
   MENARCHE 3               PREGNOWQ 3               MAYBPREG 3            
   EVERPREG 3               CURRPREG 3               MOSCURRP 3            
   HASBABES 3               CMLASTLB 4               CMLSTPRG 4            
   NPLACED 3                NDIED 3                  NADOPTV 3             
   TOTPLACD 3               OTHERKID 3               NOTHRKID 3            
   RELOTHKD 3               ADPTOTKD 3               TRYADOPT 3            
   TRYEITHR 3               NBAGECAME1 3             OTHKDFOS 3            
   RELOTHKD2 3              ADPTOTKD2 3              TRYADOPT2 3           
   TRYEITHR2 3              NBAGECAME2 3             OTHKDFOS2 3           
   RELOTHKD3 3              ADPTOTKD3 3              TRYADOPT3 3           
   TRYEITHR3 3              NBAGECAME3 3             OTHKDFOS3 3           
   RELOTHKD4 3              ADPTOTKD4 3              TRYADOPT4 3           
   TRYEITHR4 3              NBAGECAME4 3             OTHKDFOS4 3           
   RELOTHKD5 3              ADPTOTKD5 3              TRYADOPT5 3           
   TRYEITHR5 3              NBAGECAME5 3             OTHKDFOS5 3           
   RELOTHKD6 3              ADPTOTKD6 3              TRYADOPT6 3           
   TRYEITHR6 3              NBAGECAME6 3             OTHKDFOS6 3           
   RELOTHKD7 3              ADPTOTKD7 3              TRYADOPT7 3           
   TRYEITHR7 3              NBAGECAME7 3             OTHKDFOS7 3           
   RELOTHKD8 3              ADPTOTKD8 3              TRYADOPT8 3           
   TRYEITHR8 3              NBAGECAME8 3             OTHKDFOS8 3           
   RELOTHKD9 3              ADPTOTKD9 3              TRYADOPT9 3           
   TRYEITHR9 3              NBAGECAME9 3             OTHKDFOS9 3           
   RELOTHKD10 3             ADPTOTKD10 3             TRYADOPT10 3          
   TRYEITHR10 3             NBAGECAME10 3            OTHKDFOS10 3          
   RELOTHKD11 3             ADPTOTKD11 3             TRYADOPT11 3          
   TRYEITHR11 3             NBAGECAME11 3            OTHKDFOS11 3          
   RELOTHKD12 3             ADPTOTKD12 3             TRYADOPT12 3          
   TRYEITHR12 3             NBAGECAME12 3            OTHKDFOS12 3          
   RELOTHKD13 3             ADPTOTKD13 3             TRYADOPT13 3          
   TRYEITHR13 3             NBAGECAME13 3            OTHKDFOS13 3          
   RELOTHKD14 3             ADPTOTKD14 3             TRYADOPT14 3          
   TRYEITHR14 3             NBAGECAME14 3            OTHKDFOS14 3          
   RELOTHKD15 3             ADPTOTKD15 3             TRYADOPT15 3          
   TRYEITHR15 3             NBAGECAME15 3            OTHKDFOS15 3          
   RELOTHKD16 3             ADPTOTKD16 3             TRYADOPT16 3          
   TRYEITHR16 3             NBAGECAME16 3            OTHKDFOS16 3          
   RELOTHKD17 3             ADPTOTKD17 3             TRYADOPT17 3          
   TRYEITHR17 3             NBAGECAME17 3            OTHKDFOS17 3          
   RELOTHKD18 3             ADPTOTKD18 3             TRYADOPT18 3          
   TRYEITHR18 3             NBAGECAME18 3            OTHKDFOS18 3          
   EVERADPT 3               SEEKADPT 3               CONTAGEM 3            
   TRYLONG 3                KNOWADPT 3               EVWNTANO 3            
   EVCONTAG 3               TURNDOWN 3               YQUITTRY 3            
   APROCESS1 3              APROCESS2 3              APROCESS3 3           
   HRDEMBRYO 3              SRCEMBRYO_1 3            SRCEMBRYO_2 3         
   SRCEMBRYO_3 3            SRCEMBRYO_4 3            SRCEMBRYO_5 3         
   TIMESMAR 3               HSBVERIF 3               HXAGEMAR 3            
   LVTOGHX 3                STRTOGHX_Y 4             ENGAGHX 3             
   HSBRACE1 3               HSBNRACE1 3              CHEDMARN 3            
   MARBEFHX 3               KIDSHX 3                 NUMKDSHX 3            
   KIDLIVHX 3               CHKID18A 3               CHKID18B 3            
   WHRCHKDS1 3              WHRCHKDS2 3              WHRCHKDS3 3           
   SUPPORCH 3               BIOHUSBX 3               BIONUMHX 3            
   HXAGEMAR2 3              LVTOGHX2 3               STRTOGHX_Y2 4         
   ENGAGHX2 3               HSBRACE2 3               HSBNRACE2 3           
   CHEDMARN2 3              MARBEFHX2 3              KIDSHX2 3             
   NUMKDSHX2 3              KIDLIVHX2 3              CHKID18A2 3           
   CHKID18B2 3              WHRCHKDS5 3              WHRCHKDS6 3           
   SUPPORCH2 3              BIOHUSBX2 3              BIONUMHX2 3           
   HXAGEMAR3 3              LVTOGHX3 3               STRTOGHX_Y3 4         
   ENGAGHX3 3               HSBRACE3 3               HSBNRACE3 3           
   CHEDMARN3 3              MARBEFHX3 3              KIDSHX3 3             
   NUMKDSHX3 3              KIDLIVHX3 3              CHKID18A3 3           
   CHKID18B3 3              WHRCHKDS9 3              WHRCHKDS10 3          
   SUPPORCH3 3              BIOHUSBX3 3              BIONUMHX3 3           
   HXAGEMAR4 3              LVTOGHX4 3               STRTOGHX_Y4 4         
   ENGAGHX4 3               HSBRACE4 3               HSBNRACE4 3           
   CHEDMARN4 3              MARBEFHX4 3              KIDSHX4 3             
   NUMKDSHX4 3              KIDLIVHX4 3              CHKID18A4 3           
   CHKID18B4 3              WHRCHKDS13 3             WHRCHKDS14 3          
   SUPPORCH4 3              BIOHUSBX4 3              BIONUMHX4 3           
   CMMARRCH 4               PREVHUSB 3               WNSTRTCP_Y 4          
   CPHERAGE 3               CPHISAGE 3               CPENGAG1 3            
   WILLMARR 3               CURCOHRACE 3             CURCOHNRACE 3         
   CPEDUC 3                 CPMARBEF 3               CPKIDS 3              
   CPNUMKDS 3               CPKIDLIV 3               CPKID18A 3            
   CPKID18B 3               WHRCPKDS1 3              WHRCPKDS2 3           
   WHRCPKDS3 3              SUPPORCP 3               BIOCP 3               
   BIONUMCP 3               CMSTRTHP 4               LIVEOTH 3             
   EVRCOHAB 3               HMOTHMEN 3               PREVCOHB 3            
   STRTOTHX_Y 4             HERAGECX 3               HISAGECX 3            
   ENGAG1CX 3               COH1RACE 3               COH1NRACE 3           
   MAREVCX 3                CXKIDS 3                 BIOFCPX 3             
   BIONUMCX 3               STPTOGCX_Y 4             STRTOTHX_Y2 4         
   HERAGECX2 3              HISAGECX2 3              ENGAG1CX2 3           
   MAREVCX2 3               CXKIDS2 3                BIOFCPX2 3            
   BIONUMCX2 3              STPTOGCX_Y2 4            STRTOTHX_Y3 4         
   HERAGECX3 3              HISAGECX3 3              ENGAG1CX3 3           
   MAREVCX3 3               CXKIDS3 3                BIOFCPX3 3            
   BIONUMCX3 3              STPTOGCX_Y3 4            STRTOTHX_Y4 4         
   HERAGECX4 3              HISAGECX4 3              ENGAG1CX4 3           
   MAREVCX4 3               CXKIDS4 3                BIOFCPX4 3            
   BIONUMCX4 3              STPTOGCX_Y4 4            COHCHANCE 3           
   MARRCHANCE 3             PMARCOH 3                EVERSEX 3             
   RHADSEX 3                YNOSEX 3                 GRFSTSX 3             
   SXMTONCE 3               TALKPAR1 3               TALKPAR2 3            
   TALKPAR3 3               TALKPAR4 3               TALKPAR5 3            
   TALKPAR6 3               TALKPAR7 3               SEDNO 3               
   SEDNOLC1 3               SEDNOLC2 3               SEDNOLC3 3            
   SEDNOLC4 3               SEDNOG 3                 SEDNOSX 3             
   SEDBC 3                  SEDBCLC1 3               SEDBCLC2 3            
   SEDBCLC3 3               SEDBCLC4 3               SEDBCG 3              
   SEDBCSX 3                SEDWHBC 3                SEDWHLC1 3            
   SEDWHLC2 3               SEDWHLC3 3               SEDWHLC4 3            
   SEDWHBCG 3               SEDWBCSX 3               SEDCOND 3             
   SEDCONLC1 3              SEDCONLC2 3              SEDCONLC3 3           
   SEDCONLC4 3              SEDCONDG 3               SEDCONSX 3            
   SEDSTD 3                 SEDSTDLC1 3              SEDSTDLC2 3           
   SEDSTDLC3 3              SEDSTDLC4 3              SEDSTDG 3             
   SEDSTDSX 3               SEDHIV 3                 SEDHIVLC1 3           
   SEDHIVLC2 3              SEDHIVLC3 3              SEDHIVLC4 3           
   SEDHIVG 3                SEDHIVSX 3               SEDABST 3             
   SEDABLC1 3               SEDABLC2 3               SEDABLC3 3            
   SEDABLC4 3               SEDABSTG 3               SEDABSSX 3            
   SAMEMAN 3                WHOFSTPR 3               KNOWFP 3              
   STILFPSX 3               LSTSEXFP_M 3             LSTSEXFP_Y 4          
   CMLSEXFP 4               CMFPLAST 4               FPOTHREL 3            
   FPEDUC 3                 FSEXRACE 3               FSEXNRACE 3           
   FPRN 3                   WHICH1ST 3               SEXAFMEN 3            
   PTSB4MAR 4               WHOSNC1Y 3               MATCHFP 3             
   MATCHHP 3                P1YRELP 3                CMLSEX 4              
   P1YLSEX_M 3              P1YLSEX_Y 4              P1YCURRP 3            
   PCURRNT 3                MATCHFP2 3               MATCHHP2 3            
   P1YRELP2 3               CMLSEX2 4                P1YLSEX_M2 3          
   P1YLSEX_Y2 4             P1YCURRP2 3              PCURRNT2 3            
   MATCHFP3 3               MATCHHP3 3               P1YRELP3 3            
   CMLSEX3 4                P1YLSEX_M3 3             P1YLSEX_Y3 4          
   P1YCURRP3 3              PCURRNT3 3               P1YOTHREL 3           
   P1YRAGE 3                P1YHSAGE 3               P1YRF 3               
   P1YFSEX_M 3              P1YFSEX_Y 4              CMFSEX 4              
   CMFSEXTOT 4              P1YEDUC 3                P1YRACE1 3            
   P1YNRACE1 3              P1YRN 3                  P1YOTHREL2 3          
   P1YRAGE2 3               P1YHSAGE2 3              P1YRF2 3              
   P1YFSEX_M2 3             P1YFSEX_Y2 4             CMFSEX2 4             
   CMFSEXTOT2 4             P1YEDUC2 3               P1YRACE2 3            
   P1YNRACE2 3              P1YRN2 3                 P1YOTHREL3 3          
   P1YRAGE3 3               P1YHSAGE3 3              P1YRF3 3              
   P1YFSEX_M3 3             P1YFSEX_Y3 4             CMFSEX3 4             
   CMFSEXTOT3 4             P1YEDUC3 3               P1YRACE3 3            
   P1YNRACE3 3              P1YRN3 3                 CURRPRTT 3            
   CURRPRTS 3               CMCURRP1 4               CMCURRP2 4            
   CMCURRP3 3               EVERTUBS 3               ESSURE 3              
   EVERHYST 3               EVEROVRS 3               EVEROTHR 3            
   WHTOOPRC 3               ONOTFUNC 3               DFNLSTRL 3            
   ANYTUBAL 3               HYST 3                   OVAREM 3              
   OTHR 3                   ANYFSTER 3               ANYOPSMN 3            
   WHATOPSM 3               DFNLSTRM 3               ANYMSTER 3            
   ANYVAS 3                 OTHRM 3                  DATFEMOP_Y 4          
   PLCFEMOP 3               PAYRSTER1 3              PAYRSTER2 3           
   PAYRSTER3 3              RHADALL 3                HHADALL 3             
   FMEDREAS1 3              FMEDREAS2 3              FMEDREAS3 3           
   FMEDREAS4 3              FMEDREAS5 3              BCREAS 3              
   BCWHYF 3                 MINCDNNR 3               DATFEMOP_Y2 4         
   PLCFEMOP2 3              PAYRSTER6 3              PAYRSTER7 3           
   PAYRSTER8 3              RHADALL2 3               HHADALL2 3            
   FMEDREAS7 3              FMEDREAS8 3              FMEDREAS9 3           
   FMEDREAS10 3             FMEDREAS11 3             BCREAS2 3             
   BCWHYF2 3                MINCDNNR2 3              DATFEMOP_Y3 4         
   PLCFEMOP3 3              PAYRSTER11 3             PAYRSTER12 3          
   PAYRSTER13 3             RHADALL3 3               HHADALL3 3            
   FMEDREAS13 3             FMEDREAS14 3             FMEDREAS15 3          
   FMEDREAS16 3             FMEDREAS17 3             BCREAS3 3             
   BCWHYF3 3                MINCDNNR3 3              DATFEMOP_Y4 4         
   PLCFEMOP4 3              PAYRSTER16 3             PAYRSTER17 3          
   PAYRSTER18 3             RHADALL4 3               HHADALL4 3            
   FMEDREAS19 3             FMEDREAS20 3             FMEDREAS21 3          
   FMEDREAS22 3             FMEDREAS23 3             BCREAS4 3             
   BCWHYF4 3                MINCDNNR4 3              OPERSAME1 3           
   OPERSAME2 3              OPERSAME3 3              OPERSAME4 3           
   OPERSAME5 3              OPERSAME6 3              DATEOPMN_Y 4          
   WITHIMOP 3               VASJAN4YR 3              PLACOPMN 3            
   PAYMSTER1 3              PAYMSTER2 3              RHADALLM 3            
   HHADALLM 3               MEDREAS1 3               MEDREAS2 3            
   BCREASM 3                BCWHYM 3                 MINCDNMN 3            
   REVSTUBL 3               DATRVSTB_Y 4             REVSVASX 3            
   DATRVVEX_Y 4             TUBS 3                   VASECT 3              
   RSURGSTR 3               PSURGSTR 3               ONLYTBVS 3            
   RWANTRVT 3               MANWANTT 3               RWANTREV 3            
   MANWANTR 3               POSIBLPG 3               REASIMPR 3            
   POSIBLMN 3               REASIMPP 3               CANHAVER 3            
   REASDIFF1 3              REASDIFF2 3              REASDIFF3 3           
   REASDIFF4 3              REASDIFF5 3              CANHAVEM 3            
   PREGNONO 3               REASNONO1 3              REASNONO2 3           
   REASNONO3 3              RSTRSTAT 3               PSTRSTAT 3            
   LASTPER 3                TRYPREG12 3              PILL 3                
   CONDOM 3                 VASECTMY 3               DEPOPROV 3            
   WIDRAWAL 3               RHYTHM 3                 SDAYCBDS 3            
   TEMPSAFE 3               PATCH 3                  RING 3                
   MORNPILL 3               ECTIMESX 3               ECREASON1 3           
   ECREASON2 3              ECREASON3 3              ECRX 3                
   ECWHERE 3                ECWHEN 3                 OTHRMETH01 3          
   OTHRMETH02 3             OTHRMETH03 3             OTHRMETH04 3          
   OTHRMETH05 3             OTHRMETH06 3             OTHRMETH07 3          
   EVIUDTYP1 3              EVIUDTYP2 3              EVIUDTYP3 3           
   NEWMETH 3                EVERUSED 3               METHDISS 3            
   METHSTOP01 3             METHSTOP02 3             METHSTOP03 3          
   METHSTOP04 3             METHSTOP05 3             METHSTOP06 3          
   METHSTOP07 3             METHSTOP08 3             METHSTOP09 3          
   METHSTOP10 3             WHENPILL 3               REASPILL01 3          
   REASPILL02 3             REASPILL03 3             REASPILL04 3          
   REASPILL05 3             REASPILL06 3             REASPILL07 3          
   REASPILL08 3             REASPILL09 3             STOPPILL1 3           
   STOPPILL2 3              STOPPILL3 3              STOPPILL4 3           
   STOPPILL5 3              STOPPILL6 3              WHENCOND 3            
   REASCOND01 3             REASCOND02 3             REASCOND03 3          
   REASCOND04 3             REASCOND05 3             REASCOND06 3          
   REASCOND07 3             STOPCOND1 3              STOPCOND2 3           
   WHENIUD 3                TYPEIUD_1 3              TYPEIUD_2 3           
   TYPEIUD_3 3              REASIUD01 3              REASIUD02 3           
   REASIUD03 3              REASIUD04 3              REASIUD05 3           
   REASIUD06 3              REASIUD07 3              REASIUD08 3           
   STOPIUD1 3               STOPIUD2 3               STOPIUD3 3            
   STOPIUD4 3               STOPIUD5 3               LARC10 3              
   LARCREMV 3               REMOVWHY_1 3             REMOVWHY_2 3          
   REMOVWHY_3 3             REMOVWHY_4 3             REMOVWHY_5 3          
   REMOVDIF 3               REMVDIFY_1 3             REMVDIFY_2 3          
   REMVDIFY_3 3             FIRSMETH1 3              FIRSMETH2 3           
   FIRSMETH3 3              FIRSMETH4 3              NUMFIRSM 3            
   DRUGDEV 3                DRUGDEV2 3               DRUGDEV3 3            
   DRUGDEV4 3               FIRSTIME1 3              FIRSTIME2 3           
   USEFSTSX 3               CMFIRSM 4                MTHFSTSX1 3           
   MTHFSTSX2 3              MTHFSTSX3 3              MTHFSTSX4 3           
   WNFSTUSE_M 3             WNFSTUSE_Y 4             CMFSTUSE 4            
   FMETHPRS 3               FMETHPRS2 3              FMETHPRS3 3           
   FMETHPRS4 3              AGEFSTUS 3               PLACGOTF 3            
   PLACGOTF2 3              PLACGOTF3 3              PLACGOTF4 3           
   USEFRSTS 3               MTHFRSTS1 3              MTHFRSTS2 3           
   MTHFRSTS3 3              MTHFRSTS4 3              INTR_EC3 3            
   CMMONSX 4                MONSX 3                  CMMONSX2 4            
   MONSX2 3                 CMMONSX3 4               MONSX3 3              
   CMMONSX4 4               MONSX4 3                 CMMONSX5 4            
   MONSX5 3                 CMMONSX6 4               MONSX6 3              
   CMMONSX7 4               MONSX7 3                 CMMONSX8 4            
   MONSX8 3                 CMMONSX9 4               MONSX9 3              
   CMMONSX10 4              MONSX10 3                CMMONSX11 4           
   MONSX11 3                CMMONSX12 4              MONSX12 3             
   CMMONSX13 4              MONSX13 3                CMMONSX14 4           
   MONSX14 3                CMMONSX15 4              MONSX15 3             
   CMMONSX16 4              MONSX16 3                CMMONSX17 4           
   MONSX17 3                CMMONSX18 4              MONSX18 3             
   CMMONSX19 4              MONSX19 3                CMMONSX20 4           
   MONSX20 3                CMMONSX21 4              MONSX21 3             
   CMMONSX22 4              MONSX22 3                CMMONSX23 4           
   MONSX23 3                CMMONSX24 4              MONSX24 3             
   CMMONSX25 4              MONSX25 3                CMMONSX26 4           
   MONSX26 3                CMMONSX27 4              MONSX27 3             
   CMMONSX28 4              MONSX28 3                CMMONSX29 4           
   MONSX29 3                CMMONSX30 4              MONSX30 3             
   CMMONSX31 4              MONSX31 3                CMMONSX32 4           
   MONSX32 3                CMMONSX33 4              MONSX33 3             
   CMMONSX34 4              MONSX34 3                CMMONSX35 4           
   MONSX35 3                CMMONSX36 4              MONSX36 3             
   CMMONSX37 4              MONSX37 3                CMMONSX38 4           
   MONSX38 3                CMMONSX39 4              MONSX39 3             
   CMMONSX40 4              MONSX40 3                CMMONSX41 4           
   MONSX41 3                CMMONSX42 4              MONSX42 3             
   CMMONSX43 4              MONSX43 3                CMMONSX44 4           
   MONSX44 3                CMMONSX45 4              MONSX45 3             
   CMMONSX46 4              MONSX46 3                CMMONSX47 4           
   MONSX47 3                CMMONSX48 4              MONSX48 3             
   CMSTRTMC 4               CMENDMC 4                INTR_ED4A 3           
   NUMMCMON 3               MC1MONS1 4               MC1SIMSQ 3            
   MC1MONS2 4               MC1MONS3 4               DATBEGIN_M 3          
   DATBEGIN_Y 4             CMDATBEGIN 4             CURRMETH1 3           
   CURRMETH2 3              CURRMETH3 3              CURRMETH4 3           
   LSTMONMETH1 3            LSTMONMETH2 3            LSTMONMETH3 3         
   LSTMONMETH4 3            PILL_12 3                DIAPH_12 3            
   IUD_12 3                 IMPLANT_12 3             DEPO_12 3             
   CERVC_12 3               MPILL_12 3               PATCH_12 3            
   RING_12 3                METHX1 3                 METHX2 3              
   METHX3 3                 METHX4 3                 METHX5 3              
   METHX6 3                 METHX7 3                 METHX8 3              
   METHX9 3                 METHX10 3                METHX11 3             
   METHX12 3                METHX13 3                METHX14 3             
   METHX15 3                METHX16 3                METHX17 3             
   METHX18 3                METHX19 3                METHX20 3             
   METHX21 3                METHX22 3                METHX23 3             
   METHX24 3                METHX25 3                METHX26 3             
   METHX27 3                METHX28 3                METHX29 3             
   METHX30 3                METHX31 3                METHX32 3             
   METHX33 3                METHX34 3                METHX35 3             
   METHX36 3                METHX37 3                METHX38 3             
   METHX39 3                METHX40 3                METHX41 3             
   METHX42 3                METHX43 3                METHX44 3             
   METHX45 3                METHX46 3                METHX47 3             
   METHX48 3                METHX49 3                METHX50 3             
   METHX51 3                METHX52 3                METHX53 3             
   METHX54 3                METHX55 3                METHX56 3             
   METHX57 3                METHX58 3                METHX59 3             
   METHX60 3                METHX61 3                METHX62 3             
   METHX63 3                METHX64 3                METHX65 3             
   METHX66 3                METHX67 3                METHX68 3             
   METHX69 3                METHX70 3                METHX71 3             
   METHX72 3                METHX73 3                METHX74 3             
   METHX75 3                METHX76 3                METHX77 3             
   METHX78 3                METHX79 3                METHX80 3             
   METHX81 3                METHX82 3                METHX83 3             
   METHX84 3                METHX85 3                METHX86 3             
   METHX87 3                METHX88 3                METHX89 3             
   METHX90 3                METHX91 3                METHX92 3             
   METHX93 3                METHX94 3                METHX95 3             
   METHX96 3                METHX97 3                METHX98 3             
   METHX99 3                METHX100 3               METHX101 3            
   METHX102 3               METHX103 3               METHX104 3            
   METHX105 3               METHX106 3               METHX107 3            
   METHX108 3               METHX109 3               METHX110 3            
   METHX111 3               METHX112 3               METHX113 3            
   METHX114 3               METHX115 3               METHX116 3            
   METHX117 3               METHX118 3               METHX119 3            
   METHX120 3               METHX121 3               METHX122 3            
   METHX123 3               METHX124 3               METHX125 3            
   METHX126 3               METHX127 3               METHX128 3            
   METHX129 3               METHX130 3               METHX131 3            
   METHX132 3               METHX133 3               METHX134 3            
   METHX135 3               METHX136 3               METHX137 3            
   METHX138 3               METHX139 3               METHX140 3            
   METHX141 3               METHX142 3               METHX143 3            
   METHX144 3               METHX145 3               METHX146 3            
   METHX147 3               METHX148 3               METHX149 3            
   METHX150 3               METHX151 3               METHX152 3            
   METHX153 3               METHX154 3               METHX155 3            
   METHX156 3               METHX157 3               METHX158 3            
   METHX159 3               METHX160 3               METHX161 3            
   METHX162 3               METHX163 3               METHX164 3            
   METHX165 3               METHX166 3               METHX167 3            
   METHX168 3               METHX169 3               METHX170 3            
   METHX171 3               METHX172 3               METHX173 3            
   METHX174 3               METHX175 3               METHX176 3            
   METHX177 3               METHX178 3               METHX179 3            
   METHX180 3               METHX181 3               METHX182 3            
   METHX183 3               METHX184 3               METHX185 3            
   METHX186 3               METHX187 3               METHX188 3            
   METHX189 3               METHX190 3               METHX191 3            
   METHX192 3               CMMHCALX1 4              CMMHCALX2 4           
   CMMHCALX3 4              CMMHCALX4 4              CMMHCALX5 4           
   CMMHCALX6 4              CMMHCALX7 4              CMMHCALX8 4           
   CMMHCALX9 4              CMMHCALX10 4             CMMHCALX11 4          
   CMMHCALX12 4             CMMHCALX13 4             CMMHCALX14 4          
   CMMHCALX15 4             CMMHCALX16 4             CMMHCALX17 4          
   CMMHCALX18 4             CMMHCALX19 4             CMMHCALX20 4          
   CMMHCALX21 4             CMMHCALX22 4             CMMHCALX23 4          
   CMMHCALX24 4             CMMHCALX25 4             CMMHCALX26 4          
   CMMHCALX27 4             CMMHCALX28 4             CMMHCALX29 4          
   CMMHCALX30 4             CMMHCALX31 4             CMMHCALX32 4          
   CMMHCALX33 4             CMMHCALX34 4             CMMHCALX35 4          
   CMMHCALX36 4             CMMHCALX37 4             CMMHCALX38 4          
   CMMHCALX39 4             CMMHCALX40 4             CMMHCALX41 4          
   CMMHCALX42 4             CMMHCALX43 4             CMMHCALX44 4          
   CMMHCALX45 4             CMMHCALX46 4             CMMHCALX47 4          
   CMMHCALX48 4             NUMMULTX1 3              NUMMULTX2 3           
   NUMMULTX3 3              NUMMULTX4 3              NUMMULTX5 3           
   NUMMULTX6 3              NUMMULTX7 3              NUMMULTX8 3           
   NUMMULTX9 3              NUMMULTX10 3             NUMMULTX11 3          
   NUMMULTX12 3             NUMMULTX13 3             NUMMULTX14 3          
   NUMMULTX15 3             NUMMULTX16 3             NUMMULTX17 3          
   NUMMULTX18 3             NUMMULTX19 3             NUMMULTX20 3          
   NUMMULTX21 3             NUMMULTX22 3             NUMMULTX23 3          
   NUMMULTX24 3             NUMMULTX25 3             NUMMULTX26 3          
   NUMMULTX27 3             NUMMULTX28 3             NUMMULTX29 3          
   NUMMULTX30 3             NUMMULTX31 3             NUMMULTX32 3          
   NUMMULTX33 3             NUMMULTX34 3             NUMMULTX35 3          
   NUMMULTX36 3             NUMMULTX37 3             NUMMULTX38 3          
   NUMMULTX39 3             NUMMULTX40 3             NUMMULTX41 3          
   NUMMULTX42 3             NUMMULTX43 3             NUMMULTX44 3          
   NUMMULTX45 3             NUMMULTX46 3             NUMMULTX47 3          
   NUMMULTX48 3             SALMX1 3                 SALMX2 3              
   SALMX3 3                 SALMX4 3                 SALMX5 3              
   SALMX6 3                 SALMX7 3                 SALMX8 3              
   SALMX9 3                 SALMX10 3                SALMX11 3             
   SALMX12 3                SALMX13 3                SALMX14 3             
   SALMX15 3                SALMX16 3                SALMX17 3             
   SALMX18 3                SALMX19 3                SALMX20 3             
   SALMX21 3                SALMX22 3                SALMX23 3             
   SALMX24 3                SALMX25 3                SALMX26 3             
   SALMX27 3                SALMX28 3                SALMX29 3             
   SALMX30 3                SALMX31 3                SALMX32 3             
   SALMX33 3                SALMX34 3                SALMX35 3             
   SALMX36 3                SALMX37 3                SALMX38 3             
   SALMX39 3                SALMX40 3                SALMX41 3             
   SALMX42 3                SALMX43 3                SALMX44 3             
   SALMX45 3                SALMX46 3                SALMX47 3             
   SALMX48 3                SAYX1 3                  SAYX2 3               
   SAYX3 3                  SAYX4 3                  SAYX5 3               
   SAYX6 3                  SAYX7 3                  SAYX8 3               
   SAYX9 3                  SAYX10 3                 SAYX11 3              
   SAYX12 3                 SAYX13 3                 SAYX14 3              
   SAYX15 3                 SAYX16 3                 SAYX17 3              
   SAYX18 3                 SAYX19 3                 SAYX20 3              
   SAYX21 3                 SAYX22 3                 SAYX23 3              
   SAYX24 3                 SAYX25 3                 SAYX26 3              
   SAYX27 3                 SAYX28 3                 SAYX29 3              
   SAYX30 3                 SAYX31 3                 SAYX32 3              
   SAYX33 3                 SAYX34 3                 SAYX35 3              
   SAYX36 3                 SAYX37 3                 SAYX38 3              
   SAYX39 3                 SAYX40 3                 SAYX41 3              
   SAYX42 3                 SAYX43 3                 SAYX44 3              
   SAYX45 3                 SAYX46 3                 SAYX47 3              
   SAYX48 3                 SIMSEQX1 3               SIMSEQX2 3            
   SIMSEQX3 3               SIMSEQX4 3               SIMSEQX5 3            
   SIMSEQX6 3               SIMSEQX7 3               SIMSEQX8 3            
   SIMSEQX9 3               SIMSEQX10 3              SIMSEQX11 3           
   SIMSEQX12 3              SIMSEQX13 3              SIMSEQX14 3           
   SIMSEQX15 3              SIMSEQX16 3              SIMSEQX17 3           
   SIMSEQX18 3              SIMSEQX19 3              SIMSEQX20 3           
   SIMSEQX21 3              SIMSEQX22 3              SIMSEQX23 3           
   SIMSEQX24 3              SIMSEQX25 3              SIMSEQX26 3           
   SIMSEQX27 3              SIMSEQX28 3              SIMSEQX29 3           
   SIMSEQX30 3              SIMSEQX31 3              SIMSEQX32 3           
   SIMSEQX33 3              SIMSEQX34 3              SIMSEQX35 3           
   SIMSEQX36 3              SIMSEQX37 3              SIMSEQX38 3           
   SIMSEQX39 3              SIMSEQX40 3              SIMSEQX41 3           
   SIMSEQX42 3              SIMSEQX43 3              SIMSEQX44 3           
   SIMSEQX45 3              SIMSEQX46 3              SIMSEQX47 3           
   SIMSEQX48 3              USELSTP 3                WYNOLSTP 3            
   HPLSTP 3                 LSTMTHP1 3               LSTMTHP2 3            
   LSTMTHP3 3               LSTMTHP4 3               LPIUDTYP 3            
   USEFSTP 3                FSTMTHP1 3               FSTMTHP2 3            
   FSTMTHP3 3               FSTMTHP4 3               USELSTP2 3            
   WYNOLSTP2 3              HPLSTP2 3                LSTMTHP5 3            
   LSTMTHP6 3               LSTMTHP7 3               LSTMTHP8 3            
   LPIUDTYP2 3              USEFSTP2 3               FSTMTHP5 3            
   FSTMTHP6 3               FSTMTHP7 3               FSTMTHP8 3            
   USELSTP3 3               WYNOLSTP3 3              HPLSTP3 3             
   LSTMTHP9 3               LSTMTHP10 3              LSTMTHP11 3           
   LSTMTHP12 3              LPIUDTYP3 3              USEFSTP3 3            
   FSTMTHP9 3               FSTMTHP10 3              FSTMTHP11 3           
   FSTMTHP12 3              WYNOTUSE 3               HPPREGQ 3             
   DURTRY_N 3               DURTRY_P 3               WHYNOUSING1 3         
   WHYNOUSING2 3            WHYNOUSING3 3            WHYNOUSING4 3         
   WHYNOUSING5 3            WHYNOTPG1 3              WHYNOTPG2 3           
   MAINNOUSE 3              YUSEPILL1 3              YUSEPILL2 3           
   YUSEPILL3 3              YUSEPILL4 3              YUSEPILL5 3           
   YUSEPILL6 3              YUSEPILL7 3              IUDTYPE 3             
   YUSEIUD1 3               YUSEIUD2 3               YUSEIUD3 3            
   YUSEIUD4 3               YUSEIUD5 3               YUSEIUD6 3            
   EKMETHOD 3               CURBCPLC 3               NUMPILLS 3            
   CURBCPAY1 3              CURBCPAY2 3              CURBCPAY3 3           
   CURBCINS 3               NOUSEINS1 3              NOUSEINS2 3           
   NOUSEINS3 3              CURBCAMT 3               NOCOST1 3             
   NOCOST2 3                PST4WKSX 4               PSWKCOND1 3           
   PSWKCOND2 3              COND1BRK 3               COND1OFF 3            
   CONDBRFL 4               CONDOFF 4                MISSPILL 3            
   P12MOCON 3               PXNOFREQ 3               BTHCON12 3            
   MEDTST12 3               BCCNS12 3                STEROP12 3            
   STCNS12 3                EMCON12 3                ECCNS12 3             
   NUMMTH12 3               PRGTST12 3               ABORT12 3             
   PAP12 3                  PELVIC12 3               PRENAT12 3            
   PARTUM12 3               STDSVC12 3               BARRIER1 3            
   BARRIER2 3               BARRIER3 3               BARRIER4 3            
   BARRIER5 3               BARRIER6 3               NUMSVC12 3            
   NUMBCVIS 3               BC12PLCX 3               BC12PLCX2 3           
   BC12PLCX3 3              BC12PLCX4 3              BC12PLCX5 3           
   BC12PLCX6 3              BC12PLCX7 3              BC12PLCX8 3           
   BC12PLCX9 3              BC12PLCX10 3             BC12PLCX11 3          
   BC12PLCX12 3             BC12PLCX13 3             BC12PLCX14 3          
   BC12PLCX15 3             IDCLINIC 3               TALKPROV_1 3          
   TALKPROV_2 3             TALKPROV_3 3             WHYPSTD 3             
   WHYNOSTD 3               BCCLARC 3                BC12PAYX1 3           
   BC12PAYX2 3              BC12PAYX3 3              BC12PAYX4 3           
   BC12PAYX7 3              BC12PAYX8 3              BC12PAYX9 3           
   BC12PAYX10 3             BC12PAYX13 3             BC12PAYX14 3          
   BC12PAYX15 3             BC12PAYX16 3             BC12PAYX19 3          
   BC12PAYX20 3             BC12PAYX21 3             BC12PAYX22 3          
   BC12PAYX25 3             BC12PAYX26 3             BC12PAYX27 3          
   BC12PAYX28 3             BC12PAYX31 3             BC12PAYX32 3          
   BC12PAYX33 3             BC12PAYX34 3             BC12PAYX37 3          
   BC12PAYX38 3             BC12PAYX39 3             BC12PAYX40 3          
   BC12PAYX43 3             BC12PAYX44 3             BC12PAYX45 3          
   BC12PAYX46 3             BC12PAYX49 3             BC12PAYX50 3          
   BC12PAYX51 3             BC12PAYX52 3             BC12PAYX55 3          
   BC12PAYX56 3             BC12PAYX57 3             BC12PAYX58 3          
   BC12PAYX61 3             BC12PAYX62 3             BC12PAYX63 3          
   BC12PAYX64 3             BC12PAYX67 3             BC12PAYX68 3          
   BC12PAYX69 3             BC12PAYX70 3             BC12PAYX73 3          
   BC12PAYX74 3             BC12PAYX75 3             BC12PAYX76 3          
   BC12PAYX79 3             BC12PAYX80 3             BC12PAYX81 3          
   BC12PAYX82 3             BC12PAYX85 3             BC12PAYX86 3          
   BC12PAYX87 3             BC12PAYX88 3             REGCAR12_F_01 3       
   REGCAR12_F_02 3          REGCAR12_F_03 3          REGCAR12_F_04 3       
   REGCAR12_F_05 3          REGCAR12_F_06 3          REGCAR12_F_07 3       
   REGCAR12_F_08 3          REGCAR12_F_09 3          REGCAR12_F_10 3       
   REGCAR12_F_11 3          REGCAR12_F_12 3          REGCAR12_F_13 3       
   REGCAR12_F_14 3          REGCAR12_F_15 3          EVERFPC 3             
   KNDMDHLP01 3             KNDMDHLP02 3             KNDMDHLP03 3          
   KNDMDHLP04 3             KNDMDHLP05 3             KNDMDHLP06 3          
   KNDMDHLP07 3             KNDMDHLP08 3             KNDMDHLP09 3          
   LASTPAP 3                MREASPAP 3               AGEFPAP 3             
   AGEFPAP2 3               ABNPAP3 3                INTPAP 3              
   PELWPAP 3                LASTPEL 3                MREASPEL 3            
   AGEFPEL 3                AGEPEL2 3                INTPEL 3              
   EVHPVTST 3               HPVWPAP 3                LASTHPV 3             
   MREASHPV 3               AGEFHPV 3                AGEHPV2 3             
   INTHPV 3                 ASKSMOKE 3               ASKPREG 3             
   ASKFOLIC 3               TALKDM 3                 PROVRESP 3            
   PROVSAYBC 3              PROVPREBC 3              PROVINFOBC 3          
   RWANT 3                  PROBWANT 3               PWANT 3               
   JINTEND 3                JSUREINT 3               JINTENDN 3            
   JEXPECTL 3               JEXPECTS 3               JINTNEXT 3            
   INTEND 3                 SUREINT 3                INTENDN 3             
   EXPECTL 3                EXPECTS 3                INTNEXT 3             
   HLPPRG 3                 HOWMANYR 3               SEEKWHO1 3            
   SEEKWHO2 3               TYPALLPG1 3              TYPALLPG2 3           
   TYPALLPG3 3              TYPALLPG4 3              TYPALLPG5 3           
   TYPALLPG6 3              WHOTEST 3                OVUL12M 3             
   WHARTIN 3                INSEM12M 3               OTMEDHEP1 3           
   OTMEDHEP2 3              OTMEDHEP3 3              OTMEDHEP4 3           
   INSCOVPG 3               FSTHLPPG_M 3             FSTHLPPG_Y 4          
   CMPGVIS1 4               TRYLONG2 3               UNIT_TRYLONG 3        
   HLPPGNOW 3               RCNTPGH_M 3              RCNTPGH_Y 4           
   CMPGVISL 4               NUMVSTPG 3               PRGVISIT 4            
   HLPMC 3                  TYPALLMC1 3              TYPALLMC2 3           
   TYPALLMC3 3              TYPALLMC4 3              TYPALLMC5 3           
   MISCNUM 3                INFRTPRB1 3              INFRTPRB2 3           
   INFRTPRB3 3              INFRTPRB4 3              INFRTPRB5 3           
   DUCHFREQ 3               PID 3                    PIDSYMPT 3            
   PIDTX 3                  LSTPIDTX_M 3             LSTPIDTX_Y 4          
   CMPIDLST 4               DIABETES 3               GESTDIAB 3            
   UF 3                     UFSONO 3                 UFCURR 3              
   UFDIAGNOS 3              UFLIMIT 3                UFTREAT_1 3           
   UFTREAT_2 3              UFTREAT_3 3              UFTREAT_4 3           
   UFTREAT_5 3              UFTREAT_6 3              UFTREAT_7 3           
   UFTREAT_8 3              ENDO 3                   ENDOCURR 3            
   ENDODIAG 3               ENDOLIM 3                ENDOTREAT_1 3         
   ENDOTREAT_2 3            ENDOTREAT_3 3            ENDOTREAT_4 3         
   ENDOTREAT_5 3            ENDOTREAT_6 3            ENDOTREAT_7 3         
   ENDOTREAT_8 3            OVUPROB 3                PCOS 3                
   PCOSSYMP_1 3             PCOSSYMP_2 3             PCOSSYMP_3 3          
   PCOSSYMP_4 3             PCOSSYMP_5 3             PCOSSYMP_6 3          
   DEAF 3                   BLIND 3                  DIFDECIDE 3           
   DIFWALK 3                DIFDRESS 3               DIFOUT 3              
   EVRCANCER 3              AGECANCER 3              PRECANCER 3           
   MAMMOG 3                 AGEMAMM1 3               REASMAMM1 3           
   CLINEXAM 3               FAMHYST 3                MOMRISK70 3           
   MOMRISK40 3              ALCORISK 3               BFEEDRISK 3           
   DONBLOOD 3               HIVTEST 3                NOHIVTST 3            
   WHENHIV_M 3              WHENHIV_Y 4              CMHIVTST 4            
   HIVTSTYR 3               HIVRESULT 3              WHYNOGET 3            
   PLCHIV 3                 RHHIVT1 3                RHHIVT21 3            
   RHHIVT22 3               HIVTST 3                 PREPHIV 3             
   TALKDOCT 3               AIDSTALK01 3             AIDSTALK02 3          
   AIDSTALK03 3             AIDSTALK04 3             AIDSTALK05 3          
   AIDSTALK06 3             AIDSTALK07 3             AIDSTALK08 3          
   AIDSTALK09 3             AIDSTALK10 3             AIDSTALK11 3          
   RETROVIR 3               PREGHIV 3                EVERVACC 3            
   HPVSHOT1 3               HPVSEX1 3                VACCPROB 3            
   BLDPRESS 3               HIGHBP 3                 BPMEDS 3              
   NUTRINFO_1 3             NUTRINFO_2 3             NUTRINFO_3 3          
   NUTRINFO_4 3             NUTRINFO_5 3             USUALCAR 3            
   USLPLACE 3               USL12MOS 3               CURRCOV 3             
   COVERHOW01 3             COVERHOW02 3             COVERHOW03 3          
   PARINSUR 3               INS_EXCH 3               INS_PREM 3            
   COVER12 3                NUMNOCOV 3               SAMEADD 3             
   BRNOUT 3                 RELRAISD 3               ATTND14 3             
   RELCURR 3                RELTRAD 3                FUNDAM1 3             
   FUNDAM2 3                FUNDAM3 3                FUNDAM4 3             
   RELDLIFE 3               ATTNDNOW 3               MILSVC 3              
   WRK12MOS 3               FPT12MOS 3               DOLASTWK1 3           
   DOLASTWK2 3              DOLASTWK3 3              DOLASTWK4 3           
   DOLASTWK5 3              DOLASTWK6 3              RWRKST 3              
   WORKP12 3                RPAYJOB 3                RNUMJOB 3             
   RFTPTX 3                 REARNTY 3                SPLSTWK1 3            
   SPLSTWK2 3               SPLSTWK3 3               SPLSTWK4 3            
   SPLSTWK5 3               SPWRKST 3                SPPAYJOB 3            
   SPNUMJOB 3               SPFTPTX 3                SPEARNTY 3            
   SAMESEX 3                CHSUPPOR 3               REACTSLF 3            
   CHBOTHER 3               ACASILANG 3              GENHEALT 3            
   BMICAT 3                 DRWEIGH 3                TELLWGHT 3            
   WGHTSCRN 3               ENGSPEAK 3               CASIBIRTH 3           
   CASILOSS 3               CASIABOR 3               CASIADOP 3            
   NOBEDYR 3                STAYREL 3                EVSUSPEN 3            
   GRADSUSP 3               SMK100 3                 AGESMK 3              
   SMOKE12 3                SMKSTOP 3                DRINK12 3             
   UNIT30D 3                DRINK30D 3               DRINKDAY 3            
   BINGE30 3                DRNKMOST 3               BINGE12 3             
   POT12 3                  COC12 3                  CRACK12 3             
   CRYSTMTH12 3             INJECT12 3               VAGSEX 3              
   AGEVAGR 3                CONDVAG 3                WHYCONDL 3            
   GETORALM 3               GIVORALM 3               CONDFELL 3            
   ANYORAL 3                TIMING 3                 ANALSEX 3             
   CONDANAL 3               OPPSEXANY 3              OPPSEXGEN 3           
   CONDSEXL 3               OPPLIFENUM 4             OPPYEARNUM 4          
   VAGNUM12 4               ORALNUM12 4              ANALNUM12 4           
   BISEXPRT 3               NONMONOG 3               NNONMONOG1 3          
   NNONMONOG2 3             NNONMONOG3 3             MALSHT12 3            
   PROSTFRQ 3               JOHNFREQ 3               HIVMAL12 3            
   GIVORALF 3               GETORALF 3               FEMSEX 3              
   SAMESEXANY 3             SAMLIFENUM 4             SAMYEARNUM 4          
   SAMESEX1 3               FSAMEREL 3               MFLASTP 3             
   DATEAPP 3                ATTRACT 3                ORIENT_A 3            
   ORIENT_B 3               CONFCONC 3               TIMALON 3             
   RISKCHEK1 3              RISKCHEK2 3              RISKCHEK3 3           
   RISKCHEK4 3              CHLAMTST 3               STDOTHR12 3           
   STDTRT12 3               GON 3                    CHLAM 3               
   HERPES 3                 GENWARTS 3               SYPHILIS 3            
   EVRINJECT 3              EVRSHARE 3               EARNTYPE 3            
   EARN 3                   EARNDK1 3                EARNDK2 3             
   EARNDK3 3                EARNDK4 3                TOINCWMY 3            
   TOTINC 3                 FMINCDK1 3               FMINCDK2 3            
   FMINCDK3 3               FMINCDK4 3               FMINCDK5 3            
   PUBASST 3                PUBASTYP1 3              FOODSTMP 3            
   WIC 3                    HLPTRANS 3               HLPCHLDC 3            
   HLPJOB 3                 FREEFOOD 3               HUNGRY 3              
   MED_COST 3               AGER 3                   FMARITAL 3            
   RMARITAL 3               EDUCAT 3                 HIEDUC 3              
   HISPANIC 3               RACE 3                   HISPRACE2 3           
   NUMKDHH 3                NUMFMHH 3                HHFAMTYP 3            
   HHPARTYP 3               NCHILDHH 3               HHKIDTYP 3            
   CSPBBHH 3                CSPBSHH 3                CSPOKDHH 3            
   INTCTFAM 3               PARAGE14 3               EDUCMOM 3             
   AGEMOMB1 3               AGER_I 3                 FMARITAL_I 3          
   RMARITAL_I 3             EDUCAT_I 3               HIEDUC_I 3            
   HISPANIC_I 3             RACE_I 3                 HISPRACE2_I 3         
   NUMKDHH_I 3              NUMFMHH_I 3              HHFAMTYP_I 3          
   HHPARTYP_I 3             NCHILDHH_I 3             HHKIDTYP_I 3          
   CSPBBHH_I 3              CSPBSHH_I 3              CSPOKDHH_I 3          
   INTCTFAM_I 3             PARAGE14_I 3             EDUCMOM_I 3           
   AGEMOMB1_I 3             RCURPREG 3               PREGNUM 3             
   COMPREG 3                LOSSNUM 3                ABORTION 3            
   LBPREGS 3                PARITY 3                 BIRTHS5 3             
   BIRTH1 3                 OUTCOM01 3               OUTCOM02 3            
   OUTCOM03 3               OUTCOM04 3               OUTCOM05 3            
   OUTCOM06 3               OUTCOM07 3               OUTCOM08 3            
   OUTCOM09 3               OUTCOM10 3               OUTCOM11 3            
   OUTCOM12 3               OUTCOM13 3               OUTCOM14 3            
   DATEND01 4               DATEND02 4               DATEND03 4            
   DATEND04 4               DATEND05 4               DATEND06 4            
   DATEND07 4               DATEND08 4               DATEND09 4            
   DATEND10 4               DATEND11 4               DATEND12 4            
   DATEND13 4               DATEND14 4               AGEPRG01 3            
   AGEPRG02 3               AGEPRG03 3               AGEPRG04 3            
   AGEPRG05 3               AGEPRG06 3               AGEPRG07 3            
   AGEPRG08 3               AGEPRG09 3               AGEPRG10 3            
   AGEPRG11 3               AGEPRG12 3               AGEPRG13 3            
   AGEPRG14 3               DATCON01 4               DATCON02 4            
   DATCON03 4               DATCON04 4               DATCON05 4            
   DATCON06 4               DATCON07 4               DATCON08 4            
   DATCON09 4               DATCON10 4               DATCON11 4            
   DATCON12 4               DATCON13 4               DATCON14 4            
   AGECON01 3               AGECON02 3               AGECON03 3            
   AGECON04 3               AGECON05 3               AGECON06 3            
   AGECON07 3               AGECON08 3               AGECON09 3            
   AGECON10 3               AGECON11 3               AGECON12 3            
   AGECON13 3               AGECON14 3               MAROUT01 3            
   MAROUT02 3               MAROUT03 3               MAROUT04 3            
   MAROUT05 3               MAROUT06 3               MAROUT07 3            
   MAROUT08 3               MAROUT09 3               MAROUT10 3            
   MAROUT11 3               MAROUT12 3               MAROUT13 3            
   MAROUT14 3               RMAROUT01 3              RMAROUT02 3           
   RMAROUT03 3              RMAROUT04 3              RMAROUT05 3           
   RMAROUT06 3              RMAROUT07 3              RMAROUT08 3           
   RMAROUT09 3              RMAROUT10 3              RMAROUT11 3           
   RMAROUT12 3              RMAROUT13 3              RMAROUT14 3           
   MARCON01 3               MARCON02 3               MARCON03 3            
   MARCON04 3               MARCON05 3               MARCON06 3            
   MARCON07 3               MARCON08 3               MARCON09 3            
   MARCON10 3               MARCON11 3               MARCON12 3            
   MARCON13 3               MARCON14 3               RMARCON01 3           
   RMARCON02 3              RMARCON03 3              RMARCON04 3           
   RMARCON05 3              RMARCON06 3              RMARCON07 3           
   RMARCON08 3              RMARCON09 3              RMARCON10 3           
   RMARCON11 3              RMARCON12 3              RMARCON13 3           
   RMARCON14 3              CEBOW 3                  CEBOWC 3              
   DATBABY1 4               AGEBABY1 3               LASTPREG 3            
   PREG_INT1 3              PREG_INT2 3              PREG_INT3 3           
   PREG_INT4 3              RCURPREG_I 3             PREGNUM_I 3           
   COMPREG_I 3              LOSSNUM_I 3              ABORTION_I 3          
   LBPREGS_I 3              PARITY_I 3               BIRTHS5_I 3           
   OUTCOM01_I 3             OUTCOM02_I 3             OUTCOM03_I 3          
   OUTCOM04_I 3             OUTCOM05_I 3             OUTCOM06_I 3          
   OUTCOM07_I 3             OUTCOM08_I 3             OUTCOM09_I 3          
   OUTCOM10_I 3             OUTCOM11_I 3             OUTCOM12_I 3          
   OUTCOM13_I 3             OUTCOM14_I 3             DATEND01_I 3          
   DATEND02_I 3             DATEND03_I 3             DATEND04_I 3          
   DATEND05_I 3             DATEND06_I 3             DATEND07_I 3          
   DATEND08_I 3             DATEND09_I 3             DATEND10_I 3          
   DATEND11_I 3             DATEND12_I 3             DATEND13_I 3          
   DATEND14_I 3             AGEPRG01_I 3             AGEPRG02_I 3          
   AGEPRG03_I 3             AGEPRG04_I 3             AGEPRG05_I 3          
   AGEPRG06_I 3             AGEPRG07_I 3             AGEPRG08_I 3          
   AGEPRG09_I 3             AGEPRG10_I 3             AGEPRG11_I 3          
   AGEPRG12_I 3             AGEPRG13_I 3             AGEPRG14_I 3          
   DATCON01_I 3             DATCON02_I 3             DATCON03_I 3          
   DATCON04_I 3             DATCON05_I 3             DATCON06_I 3          
   DATCON07_I 3             DATCON08_I 3             DATCON09_I 3          
   DATCON10_I 3             DATCON11_I 3             DATCON12_I 3          
   DATCON13_I 3             DATCON14_I 3             AGECON01_I 3          
   AGECON02_I 3             AGECON03_I 3             AGECON04_I 3          
   AGECON05_I 3             AGECON06_I 3             AGECON07_I 3          
   AGECON08_I 3             AGECON09_I 3             AGECON10_I 3          
   AGECON11_I 3             AGECON12_I 3             AGECON13_I 3          
   AGECON14_I 3             MAROUT01_I 3             MAROUT02_I 3          
   MAROUT03_I 3             MAROUT04_I 3             MAROUT05_I 3          
   MAROUT06_I 3             MAROUT07_I 3             MAROUT08_I 3          
   MAROUT09_I 3             MAROUT10_I 3             MAROUT11_I 3          
   MAROUT12_I 3             MAROUT13_I 3             MAROUT14_I 3          
   RMAROUT01_I 3            RMAROUT02_I 3            RMAROUT03_I 3         
   RMAROUT04_I 3            RMAROUT05_I 3            RMAROUT06_I 3         
   RMAROUT07_I 3            RMAROUT08_I 3            RMAROUT09_I 3         
   RMAROUT10_I 3            RMAROUT11_I 3            RMAROUT12_I 3         
   RMAROUT13_I 3            RMAROUT14_I 3            MARCON01_I 3          
   MARCON02_I 3             MARCON03_I 3             MARCON04_I 3          
   MARCON05_I 3             MARCON06_I 3             MARCON07_I 3          
   MARCON08_I 3             MARCON09_I 3             MARCON10_I 3          
   MARCON11_I 3             MARCON12_I 3             MARCON13_I 3          
   MARCON14_I 3             RMARCON01_I 3            RMARCON02_I 3         
   RMARCON03_I 3            RMARCON04_I 3            RMARCON05_I 3         
   RMARCON06_I 3            RMARCON07_I 3            RMARCON08_I 3         
   RMARCON09_I 3            RMARCON10_I 3            RMARCON11_I 3         
   RMARCON12_I 3            RMARCON13_I 3            RMARCON14_I 3         
   CEBOW_I 3                CEBOWC_I 3               DATBABY1_I 3          
   AGEBABY1_I 3             FMARNO 3                 CSPBIOKD 3            
   MARDAT01 4               MARDAT02 4               FMAR2AGE 3            
   MARDAT03 4               FMAR3AGE 3               MARDAT04 4            
   FMAR4AGE 3               MARDIS01 4               MARDIS02 4            
   MARDIS03 4               MARDIS04 4               MAREND01 3            
   MAREND02 3               MAREND03 3               MAREND04 3            
   FMAR1AGE 3               AGEDISS1 3               AGEDD1 3              
   MAR1DISS 3               DD1REMAR 3               MAR1BIR1 4            
   MAR1CON1 4               CON1MAR1 4               B1PREMAR 3            
   COHEVER 3                EVMARCOH 3               PMARRNO 3             
   NONMARR 3                TIMESCOH 3               COHAB1 4              
   COHSTAT 3                COHOUT 3                 COH1DUR 3             
   HADSEX 3                 SEXONCE 3                SEXEVER 3             
   VRY1STAG 3               SEX1AGE 3                VRY1STSX 4            
   DATESEX1 4               SEXMAR 3                 SEX1FOR 3             
   SEXUNION 3               SEXOUT 3                 FPDUR 4               
   PARTS1YR 3               LSEXDATE 4               LSEXPREG 3            
   SEX3MO 3                 NUMP3MOS 3               LSEXRAGE 3            
   PARTDUR1 4               PARTDUR2 4               PARTDUR3 4            
   RELATP1 3                RELATP2 3                RELATP3 3             
   LIFPRTNR 3               FMARNO_I 3               CSPBIOKD_I 3          
   MARDAT01_I 3             MARDAT02_I 3             MARDAT03_I 3          
   MARDAT04_I 3             MARDIS01_I 3             MARDIS02_I 3          
   MARDIS03_I 3             MARDIS04_I 3             MAREND01_I 3          
   MAREND02_I 3             MAREND03_I 3             MAREND04_I 3          
   FMAR1AGE_I 3             AGEDISS1_I 3             AGEDD1_I 3            
   MAR1DISS_I 3             DD1REMAR_I 3             MAR1BIR1_I 3          
   MAR1CON1_I 3             CON1MAR1_I 3             B1PREMAR_I 3          
   COHEVER_I 3              EVMARCOH_I 3             PMARRNO_I 3           
   NONMARR_I 3              TIMESCOH_I 3             COHAB1_I 3            
   COHSTAT_I 3              COHOUT_I 3               COH1DUR_I 3           
   HADSEX_I 3               SEXEVER_I 3              VRY1STAG_I 3          
   SEX1AGE_I 3              VRY1STSX_I 3             DATESEX1_I 3          
   SEXONCE_I 3              SEXMAR_I 3               SEX1FOR_I 3           
   SEXUNION_I 3             SEXOUT_I 3               FPDUR_I 3             
   PARTS1YR_I 3             LSEXDATE_I 3             SEXP3MO_I 3           
   NUMP3MOS_I 3             LSEXRAGE_I 3             PARTDUR1_I 3          
   PARTDUR2_I 3             PARTDUR3_I 3             RELATP1_I 3           
   RELATP2_I 3              RELATP3_I 3              LIFPRTNR_I 3          
   STRLOPER 3               FECUND 3                 ANYBC36 3             
   NOSEX36 3                INFERT 3                 ANYBC12 3             
   ANYMTHD 3                NOSEX12 3                SEXP3MO 3             
   CONSTAT1 3               CONSTAT2 3               CONSTAT3 3            
   CONSTAT4 3               PILLR 3                  CONDOMR 3             
   SEX1MTHD1 3              SEX1MTHD2 3              SEX1MTHD3 3           
   SEX1MTHD4 3              MTHUSE12 3               METH12M1 3            
   METH12M2 3               METH12M3 3               METH12M4 3            
   MTHUSE3 3                METH3M1 3                METH3M2 3             
   METH3M3 3                METH3M4 3                FMETHOD1 3            
   FMETHOD2 3               FMETHOD3 3               FMETHOD4 3            
   DATEUSE1 6               OLDWP01 3                OLDWP02 3             
   OLDWP03 3                OLDWP04 3                OLDWP05 3             
   OLDWP06 3                OLDWP07 3                OLDWP08 3             
   OLDWP09 3                OLDWP10 3                OLDWP11 3             
   OLDWP12 3                OLDWP13 3                OLDWP14 3             
   OLDWR01 3                OLDWR02 3                OLDWR03 3             
   OLDWR04 3                OLDWR05 3                OLDWR06 3             
   OLDWR07 3                OLDWR08 3                OLDWR09 3             
   OLDWR10 3                OLDWR11 3                OLDWR12 3             
   OLDWR13 3                OLDWR14 3                WANTRP01 3            
   WANTRP02 3               WANTRP03 3               WANTRP04 3            
   WANTRP05 3               WANTRP06 3               WANTRP07 3            
   WANTRP08 3               WANTRP09 3               WANTRP10 3            
   WANTRP11 3               WANTRP12 3               WANTRP13 3            
   WANTRP14 3               WANTP01 3                WANTP02 3             
   WANTP03 3                WANTP04 3                WANTP05 3             
   WANTP06 3                WANTP07 3                WANTP08 3             
   WANTP09 3                WANTP10 3                WANTP11 3             
   WANTP12 3                WANTP13 3                WANTP14 3             
   NWWANTRP01 3             NWWANTRP02 3             NWWANTRP03 3          
   NWWANTRP04 3             NWWANTRP05 3             NWWANTRP06 3          
   NWWANTRP07 3             NWWANTRP08 3             NWWANTRP09 3          
   NWWANTRP10 3             NWWANTRP11 3             NWWANTRP12 3          
   NWWANTRP13 3             NWWANTRP14 3             WANTP5 3              
   STRLOPER_I 3             FECUND_I 3               INFERT_I 3            
   ANYMTHD_I 3              NOSEX12_I 3              SEX3MO_I 3            
   CONSTAT1_I 3             CONSTAT2_I 3             CONSTAT3_I 3          
   CONSTAT4_I 3             PILLR_I 3                CONDOMR_I 3           
   SEX1MTHD1_I 3            SEX1MTHD2_I 3            SEX1MTHD3_I 3         
   SEX1MTHD4_I 3            MTHUSE12_I 3             METH12M1_I 3          
   METH12M2_I 3             METH12M3_I 3             METH12M4_I 3          
   MTHUSE3_I 3              METH3M1_I 3              METH3M2_I 3           
   METH3M3_I 3              METH3M4_I 3              FMETHOD1_I 3          
   FMETHOD2_I 3             FMETHOD3_I 3             FMETHOD4_I 3          
   DATEUSE1_I 3             OLDWP01_I 3              OLDWP02_I 3           
   OLDWP03_I 3              OLDWP04_I 3              OLDWP05_I 3           
   OLDWP06_I 3              OLDWP07_I 3              OLDWP08_I 3           
   OLDWP09_I 3              OLDWP10_I 3              OLDWP11_I 3           
   OLDWP12_I 3              OLDWP13_I 3              OLDWP14_I 3           
   OLDWR01_I 3              OLDWR02_I 3              OLDWR03_I 3           
   OLDWR04_I 3              OLDWR05_I 3              OLDWR06_I 3           
   OLDWR07_I 3              OLDWR08_I 3              OLDWR09_I 3           
   OLDWR10_I 3              OLDWR11_I 3              OLDWR12_I 3           
   OLDWR13_I 3              OLDWR14_I 3              WANTRP01_I 3          
   WANTRP02_I 3             WANTRP03_I 3             WANTRP04_I 3          
   WANTRP05_I 3             WANTRP06_I 3             WANTRP07_I 3          
   WANTRP08_I 3             WANTRP09_I 3             WANTRP10_I 3          
   WANTRP11_I 3             WANTRP12_I 3             WANTRP13_I 3          
   WANTRP14_I 3             WANTP01_I 3              WANTP02_I 3           
   WANTP03_I 3              WANTP04_I 3              WANTP05_I 3           
   WANTP06_I 3              WANTP07_I 3              WANTP08_I 3           
   WANTP09_I 3              WANTP10_I 3              WANTP11_I 3           
   WANTP12_I 3              WANTP13_I 3              WANTP14_I 3           
   NWWANTRP01_I 3           NWWANTRP02_I 3           NWWANTRP03_I 3        
   NWWANTRP04_I 3           NWWANTRP05_I 3           NWWANTRP06_I 3        
   NWWANTRP07_I 3           NWWANTRP08_I 3           NWWANTRP09_I 3        
   NWWANTRP10_I 3           NWWANTRP11_I 3           NWWANTRP12_I 3        
   NWWANTRP13_I 3           NWWANTRP14_I 3           WANTP5_I 3            
   FPTIT12 3                FPTITMED 3               FPTITSTE 3            
   FPTITBC 3                FPTITCHK 3               FPTITCBC 3            
   FPTITCST 3               FPTITEC 3                FPTITCEC 3            
   FPTITPRE 3               FPTITABO 3               FPTITPAP 3            
   FPTITPEL 3               FPTITPRN 3               FPTITPPR 3            
   FPTITSTD 3               FPREGFP 3                FPREGMED 3            
   FPTIT12_I 3              FPTITMED_I 3             FPTITSTE_I 3          
   FPTITBC_I 3              FPTITCHK_I 3             FPTITCBC_I 3          
   FPTITCST_I 3             FPTITEC_I 3              FPTITCEC_I 3          
   FPTITPRE_I 3             FPTITABO_I 3             FPTITPAP_I 3          
   FPTITPEL_I 3             FPTITPRN_I 3             FPTITPPR_I 3          
   FPTITSTD_I 3             FPREGFP_I 3              FPREGMED_I 3          
   INTENT 3                 ADDEXP 3                 INTENT_I 3            
   ADDEXP_I 3               ANYPRGHP 3               ANYMSCHP 3            
   INFEVER 3                OVULATE 3                TUBES 3               
   INFERTR 3                INFERTH 3                ADVICE 3              
   INSEM 3                  INVITRO 3                ENDOMET 3             
   FIBROIDS 3               PIDTREAT 3               EVHIVTST 3            
   FPTITHIV 3               ANYPRGHP_I 3             ANYMSCHP_I 3          
   INFEVER_I 3              OVULATE_I 3              TUBES_I 3             
   INFERTR_I 3              INFERTH_I 3              ADVICE_I 3            
   INSEM_I 3                INVITRO_I 3              ENDOMET_I 3           
   FIBROIDS_I 3             PIDTREAT_I 3             EVHIVTST_I 3          
   FPTITHIV_I 3             CURR_INS 3               METRO 3               
   RELIGION 3               LABORFOR 3               CURR_INS_I 3          
   RELIGION_I 3             LABORFOR_I 3             POVERTY 4             
   TOTINCR 3                PUBASSIS 3               POVERTY_I 3           
   TOTINCR_I 3              PUBASSIS_I 3             SECU 3                
   SEST 4                   CMINTVW 4                CMLSTYR 4             
   CMJAN3YR 4               CMJAN4YR 4               CMJAN5YR 4            
   QUARTER 3                PHASE 3                  INTVWYEAR 4 ;
RUN ;


PROC SQL NOPRINT;
    CREATE TABLE NSFG.FemRespLabels_2017_2019 AS
    SELECT *
    FROM dictionary.columns
    WHERE libname="NSFG" AND UPCASE(memname)="FEMRESPDATA_2017_2019";
QUIT;
