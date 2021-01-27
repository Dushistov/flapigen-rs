foreign_class!(
    #[derive(PlainClass, camelCaseAliases)]
    class VecOfPaths {
        self_type Vec<PathBuf>;
        private constructor = empty;
    }
);
