foreigner_class!(class MapBitmap {
    self_type dyn Bitmap;
    private constructor = empty -> Box<Box<dyn Bitmap>>;
});

foreigner_class!(class MapBitmapGenerator {
    self_type dyn MapBitmapGenerator;
    private constructor = empty -> Box<Box<dyn MapBitmapGenerator>>;
    fn already_rendered_bitmap(&self) -> Option<Box<Box<dyn Bitmap>>> {
        this.already_rendered_bitmap().map(|bmp| {
            let bmp: Box<dyn Bitmap> = bmp;
            Box::new(bmp)
        })
    }
});
