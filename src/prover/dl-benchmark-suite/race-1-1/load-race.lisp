#+(AND :LINUX :ALLEGRO-V5.0)
(load "dl-test:race-1-1;race-1-1-acl5-linux;load-race")

#+(AND :LINUX :ALLEGRO-V5.0.1)
(load "dl-test:race-1-1;race-1-1-acl501-linux;load-race")

#+(AND :UNIX (NOT :LINUX) :ALLEGRO-VERSION>= (not (version>= 5)) (version>= 4))
(load "dl-test:race-1-1;race-1-1-acl4-solaris;load-race")

#+(AND :UNIX (NOT :LINUX) :ALLEGRO-VERSION>= (version>= 5))
(load "dl-test:race-1-1;race-1-1-acl5-solaris;load-race")
