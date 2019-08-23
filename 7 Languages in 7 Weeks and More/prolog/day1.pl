%% Jonathan Davis
%% CSC 4010 - 800 - Programming Languages
%% 7L7W - Prolog Day 1
%% Dr. Martha J. Kosa

wrote('William Shakespeare', 'Julius Caesar').
wrote('William Shakespeare', 'Hamlet').
wrote('William Shakespeare', 'Macbeth').

wrote('Stephen King', 'It').
wrote('Stephen King', 'Carrie').
wrote('Stephen King', 'The Shining').
wrote('Stephen King', 'The Dark Tower').

wrote('R. L. Stine', 'Night of the Living Dummy').
wrote('R. L. Stine', 'The Haunted Mask').
wrote('R. L. Stine', 'Say Cheese And Die!').


author1 :- write('William Shakespeare wrote: '), wrote('William Shakespeare', Title), nl, write(Title).
author2 :- write('Stephen King wrote: '), wrote('Stephen King', Title), nl, write(Title).
author3 :- write('R. L. Stine wrote: '), wrote('R. L. Stine', Title), nl, write(Title).



plays('John Lennon', 'vocals').
plays('Paul McCartney', 'bass').
plays('George Harrison', 'guitar').
plays('Ringo Starr', 'drums').

plays('Axl Rose', 'vocals').
plays('Jon Bon Jovi', 'vocals').
plays('Elvis Presley', 'vocals').

plays('Jimi Hendrix', 'guitar').
plays('Slash', 'guitar').
plays('Eddie Van Halen', 'guitar').

plays('Phil Collins', 'drums').
plays('John Paul Jones', 'bass').


genre('John Lennon', 'Multiple').
genre('Paul McCartney', 'Multiple').
genre('George Harrison', 'Multiple').
genre('Ringo Starr', 'Multiple').

genre('Axl Rose', 'Hard Rock').
genre('Jon Bon Jovi', 'Glam Metal').
genre('Elvis Presley', 'Rock').

genre('Jimi Hendrix', 'Rock').
genre('Slash', 'Hard Rock').
genre('Eddie Van Halen', 'Rock').

genre('Phil Collins', 'Pop Rock').
genre('John Paul Jones', 'Rock').


guitarists :- write('Famous guitarists include: '), plays(Person, 'guitar'), nl, write(Person), write(' whose genre is: '), genre(Person, Genre), write(Genre).
singers :- write('Famous singers include: '), plays(Person, 'vocals'), nl, write(Person), write(' whose genre is: '), genre(Person, Genre), write(Genre).
bassists :- write('Famous bassists include: '), plays(Person, 'bass'), nl, write(Person), write(' whose genre is: '), genre(Person, Genre), write(Genre).
drummers :- write('Famous drummers include: '), plays(Person, 'drums'), nl, write(Person), write(' whose genre is: '), genre(Person, Genre), write(Genre).