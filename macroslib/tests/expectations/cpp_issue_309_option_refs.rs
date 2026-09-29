// https://github.com/Dushistov/flapigen-rs/issues/309#issuecomment-614913400
foreign_class!(
    /// ID of a user.
    #[derive(Clone)]
    class UserId {
        self_type UserId;
        private constructor = empty;
        static_method user_id::validate(s: &str) -> Result<UserId, String>;
        method user_id::id(&self) -> String; alias getId;
    }
);

foreign_class!(class GroupId {
    self_type GroupId;
    private constructor = empty;
});

foreign_class!(class GroupName {
    self_type GroupName;
    private constructor = empty;
});

foreign_class!(
    /// Options for group creation.
    class GroupCreateOpts {
        self_type GroupCreateOpts;
        constructor GroupCreateOpts::default() -> GroupCreateOpts;
        constructor group_create_opts::create(
            id: Option<&GroupId>,
            name: Option<&GroupName>,
            addAsAdmin: bool,
            addAsMember: bool,
            owner: Option<&UserId>,
            admins: Vec<UserId>,
            members: Vec<UserId>,
            needsRotation: bool
        ) -> GroupCreateOpts;
    }
);
