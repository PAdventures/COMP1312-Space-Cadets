clear X;
for X lt 10; incr X do;
	clear Y;
	incr Y by 5;
end;

fn print param var then;
	set var to 10;
	decr var by 5;
end;

call print arg X arg Y;

clear Y;
incr Y by 5;
while Y gt 0 do;
	decr Y;
end;

clear Z;
incr Z;
if Z eq 1 then;
	clear Z;
elif Z lt 1 then;
	clear Z;
else then;
	clear Z;
end;

clear A;
while A le 10 do;
	incr A;
end;