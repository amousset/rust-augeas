use augeas_sys::*;

bitflags! {
    pub struct Flags: aug_flags {
        const NONE = aug_flags_AUG_NONE;
        const SAVE_BACKUP = aug_flags_AUG_SAVE_BACKUP;
        const SAVE_NEW_FILE = aug_flags_AUG_SAVE_NEWFILE;
        const TYPE_CHECK = aug_flags_AUG_TYPE_CHECK;
        const NO_STD_INCLUDE = aug_flags_AUG_NO_STDINC;
        const SAVE_NOOP = aug_flags_AUG_SAVE_NOOP;
        const NO_LOAD = aug_flags_AUG_NO_LOAD;
        const NO_MODULE_AUTOLOAD = aug_flags_AUG_NO_MODL_AUTOLOAD;
        const ENABLE_SPAN = aug_flags_AUG_ENABLE_SPAN;
        const NO_ERROR_CLOSE = aug_flags_AUG_NO_ERR_CLOSE;
        const TRACE_MODULE_LOADING = aug_flags_AUG_TRACE_MODULE_LOADING;
    }
}
