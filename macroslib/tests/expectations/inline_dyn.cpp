@@expect {"after":"\n\nprivate:\n ","before":"public:\n\n    ","file":"MapBitmapGenerator.hpp","kind":"between"}
std::optional<MapBitmap> already_rendered_bitmap() const noexcept;
@@end

@@expect {"after":"\n\n    void M","before":"extern const uintptr_t RustForeignClassMapBitmapGeneratorElemSize;\n\n    ","file":"c_MapBitmapGenerator.h","kind":"between"}
struct CRustOption4232mut3232c_void MapBitmapGenerator_already_rendered_bitmap(const MapBitmapGeneratorOpaque * const self);
@@end
