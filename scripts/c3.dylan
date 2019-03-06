// From http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.19.3910&rep=rep1&type=pdf
// (C3 linearization algorithm)

define constant compute-class-linearization =
  method (c :: <class>) => (cpl :: <list>)
    local method merge-lists (reversed-partial-result :: <list>,
                              remaining-inputs :: <sequence>)
      if (every?(empty?, remaining-inputs))
        reverse!(reversed-partial-result)
      else
        // start of selection rule
        local method candidate (c :: <class>)
          // returns c if it can go in the result now, otherwise false
          local method head? (l :: <list>)
            c == head(l)
          end method head?,
          method tail? (l :: <list>)
            member?(c, tail(l))
          end method tail?;
          any?(head?, remaining-inputs)
          & ~any?(tail?, remaining-inputs)
          & c
        end method candidate,
        method candidate-direct-superclass (c :: <class>)
          any?(candidate, direct-superclasses(c))
        end method candidate-direct-superclass;
        let next = any?(candidate-direct-superclass,
                    reversed-partial-result);
        // end of selection rule
        if (next)
          local method remove-next (l :: <list>)
            if (head(l) == next) tail(l) else l end
          end method remove-next;
          merge-lists(pair(next, reversed-partial-result),
                      map(remove-next, remaining-inputs))
        else
          error("Inconsistent precedence graph");
        end if
      end if
    end method merge-lists;

  let c-direct-superclasses = direct-superclasses(c);
  local method cpl-list (c)
     as(<list>, all-superclasses(c))
  end method cpl-list;
  merge-lists(list(c),
              concatenate(map(cpl-list, c-direct-superclasses),
                          list(as(<list>, c-direct-superclasses))));
end method; // compute-class-linearization
