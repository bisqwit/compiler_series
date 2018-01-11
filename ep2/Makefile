conj: conj.cc
	#g++ -o $@ $^ -Wall -Wextra -Ofast -g -std=c++1z -DYY_NULLPTR=nullptr
	g++ -o $@ $^ -Wall -Wextra -O0 -fsanitize=address -g -std=c++1z -DYY_NULLPTR=nullptr

conj.cc: conj.cc.re
	#re2c -d -o "$@" "$<"
	re2c -o "$@" "$<"
conj.cc.re: conj.y
	#bison "$<" -t  -o "$@" --defines="conj.hh"
	bison "$<" -t --report=all -o "$@"
	sed -i 's/new (yyas_<T> ()) T (t)/new (yyas_<T> ()) T (std\:\:move((T\&)t))/' "$@"
