/*
function Double_INT(x) { return x +_INT_INT x; }
function Double_STR(x) { return x +_STR_STR x; }
Double_INT(3) +_INT_STR Double_STR('3');
*/
function Double(x) { return x + x; }
Double(3) + Double('3');
