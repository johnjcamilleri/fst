<country_se> ::= "Sweden":"46" [0];
<country_mt>  ::= "Malta":"356" ;

<area_se> ::= "Gothenburg":"727" | "Stockholm":"689" ;
<area_mt> ::= "GoMobile":"79"    | "Vodafone":"99"   | "Landline":"21" ;

<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;

<number_se> ::= <country_se> <area_se> <digit>^6 ;
<number_mt> ::= <country_mt> <area_mt> <digit>^6 ;

<main> ::= ["Intl":"+"] [ <number_se> | <number_mt> ] ;

