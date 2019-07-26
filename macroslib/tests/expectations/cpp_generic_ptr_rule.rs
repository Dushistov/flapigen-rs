foreign_typemap!(
    define_c_type!(module="MapPoint.h";
                   #[repr(C)]
                   pub struct MapPoint {
                       pub x: f64,
                       pub y: f64,
                   }
    );
    (r_type) MapPoint;
    (f_type) "MapPoint";
);

foreign_typemap!(
    define_c_type!(module="MapRect.h";
                   #[repr(C)]
                   pub struct MapRect {
                       pub left_top: MapPoint,
                       pub right_bottom: MapPoint,
                   }
    );
    (r_type) MapRect;
    (f_type) "MapRect";
);

type MapRectRef<'a> = &'a [MapPoint];
foreign_typemap!(
    ($p:r_type) MapRectRef <= *const MapRect {
        assert!(!$p.is_null());
        let map_rect: &MapRect = unsafe { &*$p };
        let points_arr: [MapPoint; 4] = map_rect.into();
        $out = &points_arr;
    };
    ($p:f_type, req_modules = ["\"MapRect.h\""]) <= "const MapRect &" "$p";
);

foreigner_class!(class Foo {
    fn f(a: MapRectRef);
});
