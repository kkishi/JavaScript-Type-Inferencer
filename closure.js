function Foo(a) {
    function Bar() {
        return a;
    }
    return Bar();
}
var a = Foo(1);
var b = Foo('1');
