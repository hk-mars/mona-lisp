

/**
 * Here, providing an interface to create a module defined by users.
 * Load the modules(in a table) into the lisp's context.
 * Call the APIs in a module in lisp.
 *
 */



#if 0
char*
user_get_device_name(void)
{
    return "mydevice";
}


bool
user_send_message(char *msg, char *email)
{

    return true;
}


bool
eval_get_device_name(form *form, eval_value_s *result)
{
    // call user_get_device_name
    
    return true;
}


bool
eval_send_message(form *form, eval_value_s *result)
{
    // check arguments
    
    // call user_send_message
    
    return true;
}



module_user_api[] = 
{
    { "get_device_name", user_get_device_name, eval_get_device_name },
    { "send_message", user_send_message, eval_send_message },

};
		     
#endif


/*

(setq name (user.get_device_name nil))

(user.send_message "Hello Toby" "hk.mars@aol.com")

*/
