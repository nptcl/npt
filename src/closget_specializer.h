
#define stdget_specializer_object_ _n(stdget_specializer_object_)
#define stdset_specializer_object_ _n(stdset_specializer_object_)
#define stdget_specializer_type_ _n(stdget_specializer_type_)
#define stdset_specializer_type_ _n(stdset_specializer_type_)

/* eql-specializer */
int stdget_specializer_object_(addr pos, addr *ret);
int stdset_specializer_object_(addr pos, addr value);
int stdget_specializer_type_(addr pos, addr *ret);
int stdset_specializer_type_(addr pos, addr value);

