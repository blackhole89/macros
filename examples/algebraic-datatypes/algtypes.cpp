#include <stdio.h>

@include "algtypes.h"

datatype List<T> = Nil | Cons(T,List<T>) ;

/* can define mutually recursive types using stub definitions */
datatype Right;
datatype Left = SomeRight(Right) | LeftEnd;
datatype Right = SomeLeft(Left) | RightEnd;

/* syntactic sugar for specifying lists */
@define List {
    ( @^[{}][=][;][()]$template {@^[,]$head, @^$tail} ) => (
        Cons$template($head, List$template{$tail} )
    )
    ( @^[{}][=][;][()]$template {} ) => (
        Nil$template
    )
    ( @^[{}][=][;][()]$template {@^[,]$head} ) => (
        @match ($template) {
            (<@^$t) => ( Cons$template($head, Nil$template) )
            () => ( Cons($head, Nil<decltype ($head)>) ) // "infer" the right type of Nil
        }
    )
    /* fallthrough */
    () => ( @!List )
}

/* print a list */
template<class T> void print_list(List<T> l,char *fmt) 
{
    match(l) {
    case Cons(&x,&xs):
        printf(fmt,x);
        print_list(xs,fmt);
    case Nil:
        return;
    }
}

/* flatten a list of lists */
template<class T> List<T> unions(List<List<T>> ls)
{
    match(ls) {
    case Cons(&xs,Nil):
        return xs;
    case Cons(Cons(&x,&xs),&ys):
        return Cons(x, unions(Cons(xs,ys)));
    case Cons(Nil,&ys):
        return unions(ys);
    case Nil:
        return Nil<T>;
    }
}

/* some haar wavelet thing to test complex matches */
template<class T> List<T> haar(List<T> a,List<T> b=Nil<T>,List<T> c=Nil<T>)
{
	match(a,b,c) {
	case Cons(&s,Nil),Nil,&d:
		return Cons(s,d);
	case Nil,&s,&d:
		return haar(s,Nil<T>,d);
	case Cons(&h1,Cons(&h2,&t)),&s,&d:
		return haar(t,Cons(h1+h2,s),Cons(h1-h2,d));
	}
}

int main(int argc, char* argv[])
{
    auto l = List{List{1,2,3},List{4,5},List<int>{},List{6,7,8}};
    print_list(unions(l),"%d\n");

    auto l2 = haar( List{1,3,3,7} );
    print_list(l2,"%d ");

    auto test = SomeLeft(SomeRight(SomeLeft(LeftEnd)));

    return 0;
}

