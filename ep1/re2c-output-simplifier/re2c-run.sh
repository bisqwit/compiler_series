s="(?:'.'|'\\\\.')"
re2c test.re -i \
	| perl -0777 -pe 's/(yy[0-9]+:)\n/$1/gs' \
	| php case-ext.php \
	| perl -0777 -pe 's/(yych =[^;]*)\;\s+switch \(yych/switch ($1/gs' \
	| perl -0777 -pe 's/(\+\+ctx.cursor;)\s+(\{ (?:return|ctx.loc))/$1$2/gs' \
	| perl -0777 -pe 's/\{ (return|ctx.loc)/\t{ $1/gs' \
	| perl -0777 -pe 's/switch \(([^)]*)\) \{\n\tcase ('"$s"'):\tgoto ([a-z0-9]+);\n\tdefault:\tgoto ([a-z0-9]+);\n\t\}/if (($1) == $2) goto $3; else goto $4;/gs' \
	| perl -0777 -pe 's/default:\s+goto ([a-z0-9]+);\n\t\}\n\1:\s+\{ ((?:return|ctx.loc)[^\n]*)/default: $1:\t{ $2\n\t}/gs' \
	| expand -t8,26

#	| perl -0777 -pe 's/switch \(([^)]*)\) \{\n\tcase .0. ... .9.:\tgoto ([a-z0-9]+);\n\tdefault: ([a-z0-9]+:)?([^\n]*)\n\t\}/if (isdigit($1)) goto $2; else goto $3;\n$3\t$4/gs' \
#	| perl -0777 -pe 's/switch \(([^)]*)\) \{\n\tcase .0. ... .9.:\n\tcase .A. ... .Z.:\n\tcase ._.:\n\tcase .a. ... .z.:\tgoto ([a-z0-9]+);\n\tdefault: ([a-z0-9]+:)?([^\n]*)\n\t\}/if (isalnum_($1)) goto $2; else goto $3;\n$3\t$4/gs' \
#	| perl -0777 -pe 's/switch \(([^)]*)\) \{\n\tcase ('"$s"'):\n\tcase ('"$s"'):\tgoto ([a-z0-9]+);\n\tdefault:\tgoto ([a-z0-9]+);\n\t\}/if (iseither($1, $2, $3)) goto $4; else goto $5;/gs' \

#yych = *++ctx.cursor;
#        switch (yych) {


