# Recogniser for email addresses with some support for known domains

<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
<digit1> ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
<letter> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" ;
<punct> ::= "!" | "#" | "$" | "%" | "&" | "'" | "*" | "+" | "-" | "/" | "=" | "?" | "^" | "_" | "{" | "|" | "}" | "~" ;
<hyphen> ::= "-" ;

# Local part
<local_sym> ::= <digit> | <letter> | <punct> ;
# Should be limited to 64 chars, but this is impossible to compile:
# <local> ::= [<local_sym>+] - [<local_sym>^65 <local_sym>*] ;
<local> ::= <local_sym>+ ;

# IP address
<ip_segment> ::=
    <digit>
  | [<digit1> <digit>]
  | ["1" <digit>^2]
  | ["2" ["0" | "1" | "2" | "3" | "4"] <digit>]
  | ["2" "5" [<digit> - ["6" | "7" | "8" | "9"]]]
  ; 
<ip> ::= [<ip_segment> "."]^3 <ip_segment> ;

# DNS names
<hostname> ::= [<digit> | <letter> | <hyphen>]+ ;
<subdomain> ::= <hostname> ;
<domain> ::=
   [<subdomain> "."]* <hostname>
 | [(["Student":"student" | "Academic Staff":"staff"] "at":".") "Chalmers University":"chalmers"]
 | ["Apple Inc.":"apple"]
 | [("Google Mail":"mail" "at":".") "Google":"google"]
 ;

# Some known TLDs
<tld_se> ::= "Sweden":"se" ;
<tld_mt> ::= "Malta":"com.mt" ;
<tld_uk> ::= "UK":"co.uk" ;
<tld_com> ::= "com" ;
<tld_net> ::= "net" ;
<tld_org> ::= "org" ;
<tld> ::= <tld_se> | <tld_mt> | <tld_uk> | <tld_com> | <tld_net> | <tld_org> ;

# Either DNS or IP
<email> ::=
    [<local> "@" <domain> ["|":"."] <tld>]
  | [<local> "@" <ip>]
  ;

<main> ::= <email> ;

