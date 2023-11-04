check_type_error if 1 then 1 else 1;
check_type_error if true then true else 1;
check_type_error 1 + true;
check_type_error false and 1;
check_type_error 1 == true;
check_type_error not 1;
check_type_error ~ true;
check_type_error not[1, 2, 3];
check_type_error tl 2;
