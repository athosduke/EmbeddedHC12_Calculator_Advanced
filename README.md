# EmbeddedHC12_Calculator_Advanced
The calculator rules are:

Input positive decimal numbers only\
Input maximum three digit numbers only\
Valid operators are: +, -, *, and /\
Input number with leading zero is OK\
Input only two numbers and one operator in between, no spaces\
Show 'Ecalc> 'prompt and echo print user keystrokes unltil Return key\
Repeat print user input and print answer after the '=' sign\
In case of an invalid input format, repeat print the user input until the error character\
In case of an invalid input format, print error message on the next line: 'Invalid input format'\
Keep 16bit internal binary number format, detect and flag overflow error\
Use integer division and truncate any fraction

# Display Example:
The HyperTerminal display should look something like the following:\
Ecalc>\
Ecalc> 123+4\
       123+4=127\
Ecalc> 96*15\
       96*15=1440\
Ecalc> 456@5\
       456@\
       Invalid input format\
Ecalc> 7h4*12\
       7h\
       Invalid input format\
Ecalc> 3*1234\
       3*1234\
       Invalid input format	;due to 4th digit\
Ecalc> 003-678\
       003-678=-675\
Ecalc> 100+999*2\
       100+999*\
       Invalid input format\
Ecalc> 555/3\
       555/3=185\
Ecalc> 7*(45+123)\
       7*(\
       Invalid input format\
Ecalc> 78*999\
       78*999\
       Overflow error\
Ecalc> -2*123\
       -\
       Invalid input format\
Ecalc> 73/15\
       73/15=4\
Ecalc> \
