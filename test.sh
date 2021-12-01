echo "executing var X;{X=5;{if X<3 X=0 else X=X-1 |||  X=3}} eof"
echo "var X;{X=5;{if X<3 X=0 else X=X-1 |||  X=3}} eof" |./main >> test1.txt  
echo "executing var X;{X=5;{while X<3 X=X-1 |||  X=3}} eof"
echo "var X;{X=5;{while X<3 X=X-1 |||  X=3}} eof" |./main>>test2.txt
echo "executing var X;{X=5;{atom(while X<3 X=X-1 )|||  X=3}} eof"
echo "var X;{X=5;{atom(while X<3 X=X-1 )|||  X=3}} eof"|./main>>test3.txt
echo "executing var X;{X=proc Y:X=Y;X(5)}eof"
echo "var X;{X=proc Y:X=Y;X(5)}eof"|./main>>test4.txt
echo "executing var P;{P=proc Y:if Y<1 P=1 else P(Y-1);P(1)}eof"
echo "var P;{P=proc Y:if Y<1 P=1 else P(Y-1);P(1)}eof"|./main>>test5.txt
