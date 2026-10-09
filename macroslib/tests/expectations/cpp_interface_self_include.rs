struct HeaderProbe(i32);

#[repr(C)]
struct CHeaderProbe { value: i32 }

foreign_typemap!(
    foreign_code!(module = "header_probe.h";
        "struct CHeaderProbe { int32_t value; };"
    );
    (r_type) CHeaderProbe;
    (f_type, req_modules = ["\"header_probe.h\"", "\"c_SelfInclude.h\""]) "struct CHeaderProbe";
);

foreign_typemap!(
    ($p:r_type) HeaderProbe => CHeaderProbe {
        $out = CHeaderProbe { value: $p.0 };
    };
);

trait HeaderTrait {
    fn probe(&self, value: HeaderProbe);
}

foreign_callback!(callback SelfInclude {
    self_type HeaderTrait;
    onProbe = HeaderTrait::probe(&self, value: HeaderProbe);
});
