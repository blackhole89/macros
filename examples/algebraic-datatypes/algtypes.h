#include <memory>

/* Datatype definition. */
@define datatype {
    ( $typename @^[=][;]$template = @+[|]( @^[|][;]$constrs ); ) => (
        /* enum defining the identifiers for the tagged union */
        typedef enum {
            @for( $c : $constrs )[,](
                @match($c) {
                    ($cname @^$params) => ( _tag@@$cname)
                }
            )
        } _@@$typename@@Tag; 

        /* We will collect some handler code for each variant as we walk through the list,
         * to be emitted in the appropriate place. */
        @var $constructor_funs @[]      // globally visible constructor functions
        @var $constructor_branches @[]  // nontrivial constructors for variants with data
        @var $destructor_branches @[]   // nontrivial destructors for variants with data

        /* template <class T...> prefix some of our declarations will need. */
        @var $templatifier (
            @match( $template) {
                ( <@+[,]($types) >  ) => ( template < @for( $t:$types )[,](class $t) > )
                () => ()
            }
        ) 

        /* The name of the class that will represent a single box of this type. */
        @var $boxname ( _@@$typename@@Box )

        /* Define the name the user requested as an alias for a ref to the boxed type. */
        $templatifier class $boxname;
        $templatifier using $typename = std::shared_ptr<$boxname $template>;

        /* Implementation of the box class. */
        $templatifier class $boxname {
        public:
            _@@$typename@@Tag tag;

            union Data {
                /* need dummy constructors and destructors for C++ reasons */
                Data () {}
                ~Data() {}
                /* Generate a struct for each variant with data. */
                @for( $c : $constrs ) (
                    @match($c) {
                        ( $cname( @*[,]( @^[,]$params ) ) ) => (
                            /* the struct */
                            struct S@@$cname {
                                @var $counter (0)
                                @for( $p : $params)(
                                    $p p@@$counter;
                                    @set $counter (@calc($counter+1))
                                )
                            } s@@$cname;
    
                            /* push a function to generate a ref to a fresh box of this variant */
                            @push_back $constructor_funs (
                                $templatifier $typename $template $cname(
                                    @var $counter (0)
                                    @for( $p : $params )[,](
                                        $p const& a@@$counter
                                        @set $counter (@calc($counter+1))
                                    )
                                ) {
                                    auto ins = std::make_shared<$boxname $template> (_tag@@$cname);
                                    @var $counter (0)
                                    @for( $p : $params)(
                                        ins->data.s@@$cname.p@@$counter = a@@$counter;
                                        @set $counter (@calc($counter+1))
                                    )
                                    return ins;
                                }
                            )

                            /* make sure the right struct is (de)constructed */
                            @push_back $constructor_branches (
                                case _tag@@$cname:
                                    new (&data.s@@$cname) typename Data::S@@$cname; // placement new constructor
                                    break;
                            )
                            @push_back $destructor_branches (
                                case _tag@@$cname:
                                    data.s@@$cname.~S@@$cname(); // call destructor
                                    break;
                            )
                        )
                        /* Dataless variant, only needs a constructor function. */
                        ( $cname ) => (
                            @push_back $constructor_funs (
                                $templatifier const $typename $template $cname = std::make_shared<$boxname $template >(_tag@@$cname ); 
                            )
                        )
                    }
                )
            } data;

            /* constructor that constructs the right variant struct */
            $boxname( _@@$typename@@Tag fromTag ) {
                tag=fromTag;
                switch(tag) {
                    @for( $b : $constructor_branches )( $b )
                }
            }

            /* destructor that destructs the right variant struct */
            ~$boxname() {
                switch(tag) {
                    @for( $b : $destructor_branches )( $b )
                }
            }
        };
        
        /* emit globally visible constructors for end user */
        @for( $f : $constructor_funs ) ( 
            $f
        )
    )
    /* stub definition, for mutually recursive types */
    ( $typename @^[;][=]$template ; ) => (
         @var $templatifier (
            @match( $template) {
                ( <@+[,]($types) >  ) => ( template < @for( $t:$types )[,](class $t) > )
                () => ()
            }
        ) 
        @var $boxname ( _@@$typename@@Box )

        $templatifier class $boxname;
        $templatifier using $typename = std::shared_ptr<$boxname $template>;
    )
}

/* Recursive unifier for single case: statements.
 * Outputs the Boolean condition that checks we have the right variants and values in each place,
 * and stores a list of bindings for captured variables (&name)
 * that are valid if the condition succeeds. */
@define match_condition {
    ( ( @^[,]$context , @^$target ) ) => (
        @match( $target ) {
            /* capture a var */
            ( &@#$capture ) => (
                @push_back $binders ( auto &$capture = $context; )
                true
            )
            /* assume: name(...) is a variant with data */
            ( @#$cname( @*[,]( @^[,]$fields ) ) ) => (
                @var $counter (0)
                   ($context->tag == _tag@@$cname)
                && (@for( $f : $fields )[&&](
                        /* PROBLEM: Why do we need this? Why are "macro args" not expanded in the current frame? */
                        @var $temp ( $context->data.s@@$cname.p@@$counter )
                        match_condition( $temp, $f) 
                        @set $counter (@calc($counter+1))
                   )) 
            )
            /* assume: bare name is a variant without data */
            ( @#$cname ) => ( ($context->tag == _tag@@$cname) )
            /* anything else is taken to be a value to compare for equality */
            ( @^$value ) => ( ($context == ($value)) )
        }
    )
}

/* match statement. Unlike switch, there is no case fallthrough. 
 * Format. match( input1, ..., input n) { case pattern11,...,pattern1n: ... case pattern m1,...,patternmn: ... [default:] ... } */
@define match {
    ( ( @*[,]( @^[,]$args ) ) {
        @*(
            case @*[,]( @^[:][,]$patternses ): @^[case][default]$cbodies
        )
        @*(
            default: @^[case][default]$dbodies
        )
    } ) => ( {
        /* Collect inputs in a0, ..., a{n-1}, store their names in $arg_instances */
        @var $arg_instances @[]
        @var $counter (0)
        @for( $a : $args )(
            auto &a@@$counter = $a;
            @push_back $arg_instances (a@@$counter)
            @set $counter (@calc($counter+1))
        )

        /* We override case ...: and default: inside the statement body.
         * TODO: make sure switch statements inside a match statement work. */
        @var $maybeelse ()
        @for( $patterns,$cbody : $patternses, $cbodies )(
            @var $binders @[]
            /* Recursively generate a unifier for every pattern. */
            $maybeelse if( @for( $ai,$pa: $arg_instances, $patterns)[&&]( match_condition($ai,$pa) )) {
                /* Since unification succeeded, we can now bind all captured vars. */
                @for($b:$binders)( $b )
                /* Execute case body. */
                $cbody
            }
            /* ifs of future case statements should be preceded with else */
            @set $maybeelse (else)
        )

        @for( $dbody : $dbodies )(
            $maybeelse { $dbody }
        )
    } )
}

