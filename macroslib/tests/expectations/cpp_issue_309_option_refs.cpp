@@expect {"file":"GroupCreateOpts.hpp","kind":"between","before":"    GroupCreateOptsWrapper() noexcept\n    {\n\n        this->self_ = GroupCreateOpts_default();\n        if (this->self_ == nullptr) {\n            std::abort();\n        }\n    }\n\n","after":"\nprivate:\n   static void free_mem(SelfType &p) noexcept"}
    GroupCreateOptsWrapper(const GroupId * id, const GroupName * name, bool addAsAdmin, bool addAsMember, const UserId * owner, RustForeignVecUserId admins, RustForeignVecUserId members, bool needsRotation) noexcept
    {

        struct CRustClassOptGroupId a0 = CRustClassOptGroupId { (id != nullptr) ? static_cast<GroupIdOpaque *>(* id) : nullptr };

        struct CRustClassOptGroupName a1 = CRustClassOptGroupName { (name != nullptr) ? static_cast<GroupNameOpaque *>(* name) : nullptr };

        struct CRustClassOptUserId a4 = CRustClassOptUserId { (owner != nullptr) ? static_cast<UserIdOpaque *>(* owner) : nullptr };

        this->self_ = GroupCreateOpts_create(std::move(a0), std::move(a1), static_cast<char>(addAsAdmin ? 1 : 0), static_cast<char>(addAsMember ? 1 : 0), std::move(a4), admins.release(), members.release(), static_cast<char>(needsRotation ? 1 : 0));
        if (this->self_ == nullptr) {
            std::abort();
        }
    }

@@end

@@expect {"file":"rust_option.h","kind":"item","name":"CRustClassOptGroupId","form":"definition"}
struct CRustClassOptGroupId {
    const void * p;
};
@@end
