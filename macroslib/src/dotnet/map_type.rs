use crate::error::*;
use crate::{
    typemap::{
        ast::{DisplayToTokens, TyParamsSubstList},
        ty::{ForeignTypeS, RustType, TraitNamesSet},
        utils::{ForeignMethodSignature, ForeignTypeInfoT, self},
        ForeignTypeInfo, MapToForeignFlag, TypeMap, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE, TypeMapConvRuleInfoExpanderHelper, ExpandedFType, TypeMapConvRuleInfo, CItem,
    },
    types::{FnArg, ForeignerClassInfo, ForeignerMethod, MethodVariant, SelfTypeVariant},
};
use log::{debug, trace, warn};
use petgraph::Direction;
use smol_str::SmolStr;
use std::{collections::HashMap, rc::Rc, io::Write};
use syn::Type;
use rustc_hash::FxHashSet;
use super::DotNetGenerator;
use quote::ToTokens;

pub(crate) struct DotNetForeignMethodSignature {
    pub(crate) output: DotNetArgInfo,
    pub(crate) input: Vec<DotNetArgInfo>,
    pub(crate) name: String,
    pub(crate) variant: MethodVariant,
    pub(crate) rust_function_call: String,
}

impl ForeignMethodSignature for DotNetForeignMethodSignature {
    type FI = DotNetArgInfo;
    fn output(&self) -> &dyn ForeignTypeInfoT {
        &self.output
    }
    fn input(&self) -> &[DotNetArgInfo] {
        &self.input[..]
    }
}

fn calc_this_type_for_method(_: &TypeMap, class: &ForeignerClassInfo) -> Option<Type> {
    class
        .self_desc
        .as_ref()
        .map(|x| x.constructor_ret_type.clone())
}

#[derive(Debug, PartialEq, Clone)]
pub enum ArgName {
    SelfArg,
    Named(SmolStr),
    Return,
}

impl ArgName {
    pub fn rust_variable_name(&self) -> &str {
        match self {
            ArgName::SelfArg => "this",
            ArgName::Return => "ret",
            ArgName::Named(name) => name,
        }
    }

    pub fn dotnet_variable_name(&self) -> &str {
        match self {
            ArgName::SelfArg => "__this",
            ArgName::Return => "__ret",
            ArgName::Named(name) => name,
        }
    }
}

pub(crate) struct DotNetArgInfo {
    // pub foreign_type: ForeignTypeInfo,
    // pub rust_intermediate_type: RustType,
    // pub dotnet_intermediate_type: String,
    // pub rust_conversion_code: String,
    // pub dotnet_conversion_code: String,
    // pub finalizer: String,
    pub(crate) type_info: DotNetTypeInfo,
    pub(crate) arg_name: ArgName,
    pub(crate) span: SourceIdSpan,
}

impl DotNetArgInfo {
    pub(crate) fn new(type_info: DotNetTypeInfo, arg_name: ArgName, span: SourceIdSpan) -> Self {
        Self {
            type_info,
            arg_name,
            span,
        }
    }

    pub(crate) fn rust_conversion_code(&self, conv_map: &mut TypeMap) -> Result<String> {
        let direction = self.direction();
        let (from, to) = match direction {
            Direction::Incoming => (
                self.type_info.rust_intermediate_type.to_idx(),
                self.type_info.rust_type.to_idx(),
            ),
            Direction::Outgoing => (
                self.type_info.rust_type.to_idx(),
                self.type_info.rust_intermediate_type.to_idx(),
            ),
        };
        let (_cur_deps, rust_conversion_code) = conv_map.convert_rust_types(
            from,
            to,
            self.arg_name.rust_variable_name(),
            self.arg_name.rust_variable_name(),
            "()", // todo
            self.span,
        )?;
        Ok(rust_conversion_code)
    }

    pub fn dotnet_conversion_code(&self, name_generator: &mut NameGenerator) -> String {
        if self.type_info.dotnet_conversion_code.is_empty() {
            return String::new();
        }
        let dotnet_arg_name = name_generator.last_variant(self.arg_name.dotnet_variable_name());
        let new_dotnet_arg_name = name_generator.new_variant(self.arg_name.dotnet_variable_name());

        format!(
            "var {} = {};",
            new_dotnet_arg_name,
            self.type_info.dotnet_conversion_code
                .to_string()
                .replace(FROM_VAR_TEMPLATE, &dotnet_arg_name)
        )
    }

    pub fn dotnet_finalizer(&self, name_generator: &mut NameGenerator) -> String {
        if self.type_info.finalizer.is_empty() {
            return String::new();
        }

        self.type_info
            .finalizer
            .clone()
            .replace(TO_VAR_TEMPLATE, &name_generator.last_variant(self.arg_name.dotnet_variable_name()))
    }

    pub fn has_finalizer(&self) -> bool {
        !self.type_info.finalizer.is_empty()
    }

    pub fn direction(&self) -> Direction {
        if self.arg_name == ArgName::Return {
            Direction::Outgoing
        } else {
            Direction::Incoming
        }
    }
}

impl ForeignTypeInfoT for DotNetArgInfo {
    fn name(&self) -> &str {
        &self.type_info.dotnet_type
    }
    fn correspoding_rust_type(&self) -> &RustType {
        &self.type_info.rust_type
    }
}

pub(crate) struct DotNetTypeInfo {
    pub(crate) dotnet_type: SmolStr,
    pub(crate) rust_type: RustType,
    pub(crate) dotnet_intermediate_type: SmolStr,
    pub(crate) rust_intermediate_type: RustType,
    // pub rust_conversion_code: String,
    pub(crate) dotnet_conversion_code: String,
    pub(crate) finalizer: String,
}

impl DotNetTypeInfo {
    fn new_primitive(dotnet_type: SmolStr, rust_type: RustType) -> Self {
        Self {
            dotnet_intermediate_type: dotnet_type.clone(),
            rust_intermediate_type: rust_type.clone(),
            dotnet_type,
            rust_type,
            // rust_conversion_code: String::new(),
            dotnet_conversion_code: String::new(),
            finalizer: String::new(),
        }
    }
}

pub struct NameGenerator {
    map: HashMap<String, usize>,
}

impl NameGenerator {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn new_variant(&mut self, name_base: &str) -> String {
        let i = self.map.entry(name_base.to_owned()).or_insert(0);
        *i += 1;
        format!("{}_{}", name_base, i)
    }

    pub fn last_variant(&mut self, name_base: &str) -> String {
        let i = self.map.entry(name_base.to_owned()).or_insert(0);
        format!("{}_{}", name_base, i)
    }

    pub fn first_variant(name_base: &str) -> String {
        format!("{}_{}", name_base, 0)
    }
}

pub(crate) fn make_foreign_method_signature(
    generator: &mut DotNetGenerator,
    class: &ForeignerClassInfo,
    method: &ForeignerMethod,
) -> Result<DotNetForeignMethodSignature> {
    let dummy_ty = parse_type! { () };
    let dummy_rust_ty = generator.conv_map.find_or_alloc_rust_type_no_src_id(&dummy_ty);

    let input = method
        .fn_decl
        .inputs
        .iter()
        .map(|arg| match arg {
            FnArg::Default(named_arg) => Ok(DotNetArgInfo::new(
                map_type(
                    generator,
                    &named_arg.ty,
                    Direction::Incoming,
                    (class.src_id, named_arg.span),
                )?,
                ArgName::Named(named_arg.name.clone()),
                (class.src_id, named_arg.span)
            )),
            FnArg::SelfArg(span_ref, self_variant) => {
                let span = *span_ref;
                let self_ty = class.self_desc.as_ref().ok_or_else(|| {
                    DiagnosticError::new(class.src_id, span, "Non-static methods can only be defined for a class that have a constructor (at least private empty one)")
                })?.self_type.clone();
                let self_ty_full = match self_variant {
                    SelfTypeVariant::Rptr => parse_type_spanned_checked!(span, & #self_ty),
                    SelfTypeVariant::RptrMut => parse_type_spanned_checked!(span, &mut #self_ty),
                    _ => unimplemented!("Passing self by value not implemented yet"),
                };
                Ok(DotNetArgInfo::new(
                    map_type(
                        generator,
                        &self_ty_full,
                        Direction::Incoming,
                        (class.src_id, span),
                    )?,
                    ArgName::SelfArg,
                    (class.src_id, span),
                ))
            }
        })
        .collect::<Result<Vec<_>>>()?;

    let output = match method.fn_decl.output {
        syn::ReturnType::Default => DotNetArgInfo::new(
            DotNetTypeInfo::new_primitive("void".into(), dummy_rust_ty.clone()),
            ArgName::Return,
            (class.src_id, class.span())
        ),
        syn::ReturnType::Type(_, ref ty) => DotNetArgInfo::new(
            map_type(
                generator,
                ty,
                Direction::Outgoing,
                (class.src_id, class.span()),
            )?,
            ArgName::Return,
            (class.src_id, class.span())
        )
    };
    Ok(DotNetForeignMethodSignature {
        input,
        output,
        name: method.short_name(),
        variant: method.variant.clone(),
        rust_function_call: method.generate_code_to_call_rust_func(),
    })
}

fn map_type(
    generator: &mut DotNetGenerator,
    ty: &Type,
    direction: Direction,
    span: SourceIdSpan,
) -> Result<DotNetTypeInfo> {
    let rust_ty = generator.conv_map.find_or_alloc_rust_type(ty, span.0);

    let foreign_type = find_foreign_type(generator, &rust_ty, direction, span)?;
    let rule = match direction {
        Direction::Outgoing => foreign_type.into_from_rust.as_ref(),
        Direction::Incoming => foreign_type.from_into_rust.as_ref(),
    }
    .ok_or_else(|| {
        DiagnosticError::new2(
            span,
            format!(
                "No rule to convert foreign type {} as input/output type",
                foreign_type.name
            ),
        )
    })?;

    let correspoding_rust_type = generator.conv_map[rule.rust_ty].clone();

    if let Some(intermediate) = rule.intermediate.as_ref() {
        let intermediate_rust_type = generator.conv_map[intermediate.intermediate_ty].clone();
        let intermediate_foreign_type =
            find_foreign_type(generator, &intermediate_rust_type, direction, span)?;

        // let (from, to) = match direction {
        //     Direction::Incoming => (
        //         intermediate_rust_type.to_idx(),
        //         correspoding_rust_type.to_idx(),
        //     ),
        //     Direction::Outgoing => (
        //         correspoding_rust_type.to_idx(),
        //         intermediate_rust_type.to_idx(),
        //         // format!(
        //         //     "var {0} = {1}({0});",
        //         //     arg_name.dotnet_variable_name(),
        //         //     foreign_type.name
        //         // ),
        //     ),
        // };
        // let (_cur_deps, rust_conversion_code) = conv_map.convert_rust_types(
        //     from,
        //     to,
        //     arg_name.rust_variable_name(),
        //     arg_name.rust_variable_name(),
        //     "()", // todo
        //     span,
        // )?;

        // let dotnet_arg_name = name_generator.last_variant(arg_name.dotnet_variable_name());
        // let new_dotnet_arg_name = name_generator.new_variant(arg_name.dotnet_variable_name());

        // let dotnet_conversion_code = format!(
        //     "var {} = {};",
        //     new_dotnet_arg_name,
        //     intermediate
        //         .conv_code
        //         .to_string()
        //         .replace(FROM_VAR_TEMPLATE, &dotnet_arg_name)
        // );

        // let finalizer = intermediate
        //     .finalizer_code
        //     .as_ref()
        //     .map(|str| {
        //         str.as_str()
        //             .to_owned()
        //             .replace(TO_VAR_TEMPLATE, &new_dotnet_arg_name)
        //     })
        //     .unwrap_or_default();

        Ok(DotNetTypeInfo {
            dotnet_type: foreign_type.typename(),
            rust_type: correspoding_rust_type.clone(),
            dotnet_intermediate_type: intermediate_foreign_type.name.typename,
            rust_intermediate_type: intermediate_rust_type,
            dotnet_conversion_code: intermediate.conv_code.to_string(),
            finalizer: intermediate.finalizer_code.as_ref().map(|str| str.as_str().to_owned()).unwrap_or_default(),
        })
    } else {
        Ok(DotNetTypeInfo {
            dotnet_type: foreign_type.typename(),
            rust_type: correspoding_rust_type.clone(),
            rust_intermediate_type: correspoding_rust_type.clone(),
            dotnet_intermediate_type: foreign_type.typename(),
            dotnet_conversion_code: FROM_VAR_TEMPLATE.to_owned(),
            finalizer: String::new(),
        })
    }
}

fn find_foreign_type(
    generator: &mut DotNetGenerator,
    rust_ty: &RustType,
    direction: Direction,
    arg_ty_span: SourceIdSpan,
) -> Result<ForeignTypeS> {
    if let Some(foreign_type_idx) = generator.conv_map.map_through_conversation_to_foreign(
        rust_ty.to_idx(),
        direction,
        MapToForeignFlag::FastSearch,
        arg_ty_span,
        calc_this_type_for_method,
    ) {
        return Ok(generator.conv_map[foreign_type_idx].clone());
    }

    let idx_subst_map: Option<(Rc<_>, TyParamsSubstList)> =
        generator.conv_map.generic_rules().iter().find_map(|grule| {
            grule
                .is_ty_subst_of_my_generic_rtype(&rust_ty.ty, direction, |ty, traits| -> bool {
                    is_ty_implement_traits(generator.conv_map, ty, traits)
                })
                .map(|sm| (grule.clone(), sm.into()))
        });
    if let Some((grule, subst_list)) = idx_subst_map {
        debug!(
            "do_map_type: we found generic rule for {}: {:?}",
            rust_ty, subst_list
        );
        let subst_map = subst_list.as_slice().into();
        // panic!("{:#?}", subst_map);
        let c_types = grule
            .subst_generic_params_to_c_items(
                &subst_map,
                &mut DotNetGenericParamExpander {
                    generator,
                    arg_ty_span,
                    direction,
                },
            )
            .map_err(|err| {
                err.add_span_note(
                    (grule.src_id, grule.span),
                    "subst. of generic params in define_c_type failed",
                )
            })?;
                    
        if let Some(c_items) = c_types {
            if !generator.known_c_items_modules.contains(&c_items.header_name) {
                generator.known_c_items_modules.insert(c_items.header_name.clone());
                for c_item in c_items.items {
                    if let CItem::Fn(fn_item) = c_item {
                        generator.rust_code.push(fn_item.into_token_stream())
                    }
                }
            }
        }
        // if let Some(c_types) = c_types {
        //     merge_c_types(
        //         ctx,
        //         c_types,
        //         MergeCItemsFlags::DefineAlsoRustType,
        //         grule.src_id,
        //     )?;
        // }
        let new_rule = grule
            .subst_generic_params(
                subst_map,
                direction,
                &mut DotNetGenericParamExpander {
                    generator,
                    arg_ty_span,
                    direction,
                },
            )
            .map_err(|err| {
                err.add_span_note(
                    (grule.src_id, grule.span),
                    "subst. of generic params into rule failed",
                )
            })?;
        debug_assert!(!new_rule.is_empty());
        merge_rule(generator, new_rule)?;
        if let Some(foreign_type_idx) = generator.conv_map.map_through_conversation_to_foreign(
            rust_ty.to_idx(),
            direction,
            MapToForeignFlag::FullSearch,
            arg_ty_span,
            calc_this_type_for_method,
        ) {
            return Ok(generator.conv_map[foreign_type_idx].clone());
        }
    }

    if let Some(foreign_type_idx) = generator.conv_map.map_through_conversation_to_foreign(
        rust_ty.to_idx(),
        direction,
        MapToForeignFlag::FullSearch,
        arg_ty_span,
        calc_this_type_for_method,
    ) {
        return Ok(generator.conv_map[foreign_type_idx].clone());
    }

    Err(DiagnosticError::new2(
        arg_ty_span,
        format!(
            "Cannot found a conversion for a type {}",
            rust_ty.to_string()
        ),
    ))
}

fn is_ty_implement_traits(tmap: &TypeMap, ty: &syn::Type, traits: &TraitNamesSet) -> bool {
    if let Some(rty) = tmap.ty_to_rust_type_checked(ty) {
        for tname in traits.iter() {
            if tname.is_ident("SwigTypeIsReprC") {
                if tmap
                    .find_foreign_type_related_to_rust_ty(rty.to_idx())
                    .is_none()
                {
                    return false;
                }
            } else if !rty.implements.contains_path(tname) {
                return false;
            }
        }
        true
    } else {
        println!(
            "warning=mapping types: type {} unknown",
            DisplayToTokens(ty)
        );
        false
    }
}

fn merge_rule(generator: &mut DotNetGenerator, mut rule: TypeMapConvRuleInfo) -> Result<()> {
    debug!("merge_rule begin {:?}", rule);
    if rule.is_empty() {
        return Err(DiagnosticError::new(
            rule.src_id,
            rule.span,
            format!("rule {:?} is empty", rule),
        ));
    }
    // let all_options = {
    //     let mut opts = FxHashSet::<&'static str>::default();
    //     opts.extend(CppOptional::iter().map(|x| -> &'static str { x.into() }));
    //     opts.extend(CppVariant::iter().map(|x| -> &'static str { x.into() }));
    //     opts.extend(CppStrView::iter().map(|x| -> &'static str { x.into() }));
    //     opts
    // };

    // validate_cfg_options(&rule, &all_options)?;
    // let options = {
    //     let mut opts = FxHashSet::<&'static str>::default();
    //     opts.insert(ctx.cfg.cpp_variant.into());
    //     opts.insert(ctx.cfg.cpp_optional.into());
    //     opts.insert(ctx.cfg.cpp_str_view.into());
    //     opts
    // };

    // if let Some(c_types) = rule.c_types.take() {
    //     merge_c_types(ctx, c_types, MergeCItemsFlags::DefineOnlyCItem, rule.src_id)?;
    // }

    // let f_codes = mem::replace(&mut rule.f_code, vec![]);
    // panic!("{:#?}\n{:?}", rule.c_types, rule.f_code);


    for cs_code in rule.f_code.drain(..) {
        let native_lib_name = &generator.config.native_lib_name;
        generator.additional_cs_code_for_types.entry(cs_code.module_name.clone()).or_insert_with(||
            cs_code.code.replace("{native_lib_name}", native_lib_name)
        );
        // if !generator.additional_cs_code_for_types.contains_key(cs_code.module_name) {
        //     generator.additional_cs_code_for_types[cs_code.module_name]
        // }
        // generator.additional_cs_code += &cs_code.code.replace("{native_lib_name}", &generator.config.native_lib_name);
    }
    // for fcode in f_codes {
    //     let module_name = &fcode.module_name;
    //     let common_files = &mut ctx.common_files;
    //     let c_header_f = file_for_module!(ctx, common_files, module_name);
    //     let use_fcode = fcode
    //         .cfg_option
    //         .as_ref()
    //         .map(|opt| options.contains(opt.as_str()))
    //         .unwrap_or(true);

    //     if use_fcode {
    //         c_header_f
    //             .write_all(
    //                 fcode
    //                     .code
    //                     .replace("$RUST_SWIG_USER_NAMESPACE", &ctx.cfg.namespace_name)
    //                     .as_bytes(),
    //             )
    //             .map_err(DiagnosticError::map_any_err_to_our_err)?;
    //     }
    // }

    let options = FxHashSet::default();
    // rule.c_types = None;
    // rule.f_code.clear();

    utils::configure_ftype_rule(&mut rule.ftype_left_to_right, "=>", rule.src_id, &options)?;
    utils::configure_ftype_rule(&mut rule.ftype_right_to_left, "<=", rule.src_id, &options)?;

    generator.conv_map.merge_conv_rule(rule.src_id, rule)?;
    Ok(())
}

struct DotNetGenericParamExpander<'a, 'b> {
    generator: &'a mut DotNetGenerator<'b>,
    arg_ty_span: SourceIdSpan,
    direction: Direction,
}

impl<'a, 'b> DotNetGenericParamExpander<'a, 'b> {
    fn arg_direction(&self, param1: Option<&str>) -> Result<Direction> {
        match param1 {
            Some("output") => Ok(Direction::Outgoing),
            Some("input") => Ok(Direction::Incoming),
            None => Ok(self.direction),
            Some(param) => Err(DiagnosticError::new2(
                self.arg_ty_span,
                format!("Invalid argument '{}' for swig_f_type", param),
            )),
        }
    }
}

impl<'a, 'b> TypeMapConvRuleInfoExpanderHelper for DotNetGenericParamExpander<'a, 'b> {
    fn swig_i_type(&mut self, ty: &syn::Type, opt_arg: Option<&str>) -> Result<syn::Type> {
        let rust_ty = self
            .generator
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let direction = self.arg_direction(opt_arg)?;
        // let f_info = find_foreign_type(self.conv_map, &rust_ty, direction, self.arg_ty_span)?;
//        let f_info = map_type(self.conv_map, &rust_ty, direction, self.arg_ty_span)?;
        // let intermediate_rust_type = match direction {
        //     Direction::Incoming => f_info.from_into_rust
        // }
        let type_info = map_type(self.generator, &rust_ty.ty, direction, self.arg_ty_span)?;
        trace!("swig_i_type return {}", type_info.rust_intermediate_type);
        Ok(type_info.rust_intermediate_type.ty.clone())
    }
    fn swig_from_rust_to_i_type(
        &mut self,
        ty: &syn::Type,
        in_var_name: &str,
        out_var_name: &str,
    ) -> Result<String> {
        let rust_ty = self
            .generator
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let type_info = map_type(self.generator, &rust_ty.ty, Direction::Outgoing, self.arg_ty_span)?;

        let (_conv_deps, conv_code) = self.generator.conv_map.convert_rust_types(
            type_info.rust_type.to_idx(),
            type_info.rust_intermediate_type.to_idx(),
            in_var_name,
            out_var_name,
            "#error",
            self.arg_ty_span,
        )?;
        //self.ctx.rust_code.append(&mut conv_deps);
        Ok(conv_code)
    }
    fn swig_from_i_type_to_rust(
        &mut self,
        ty: &syn::Type,
        in_var_name: &str,
        out_var_name: &str,
    ) -> Result<String> {
        let rust_ty = self
            .generator
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let type_info = map_type(self.generator, &rust_ty.ty, Direction::Incoming, self.arg_ty_span)?;

        let ( _conv_deps, conv_code) = self.generator.conv_map.convert_rust_types(
            type_info.rust_intermediate_type.to_idx(),
            type_info.rust_type.to_idx(),
            in_var_name,
            out_var_name,
            "#error",
            self.arg_ty_span,
        )?;
        // self.ctx.rust_code.append(&mut conv_deps);
        Ok(conv_code)
    }
    fn swig_f_type(&mut self, ty: &syn::Type, param1: Option<&str>) -> Result<ExpandedFType> {
        let rust_ty = self
            .generator
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);

        let direction = self.arg_direction(param1)?;
        let type_info = map_type(self.generator, &rust_ty.ty, direction, self.arg_ty_span)?;
        // let fname = if let Some(ref cpp_conv) = type_info.cpp_converter {
        //     cpp_conv.typename.as_str()
        // } else {
        //     f_info.base.name.as_str()
        // };
        Ok(ExpandedFType {
            name: type_info.dotnet_type,
            provides_by_module: vec![],
        })
    }
    fn swig_foreign_to_i_type(&mut self, ty: &syn::Type, var_name: &str) -> Result<String> {
        let rust_ty = self
            .generator
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let type_info = map_type(self.generator, &rust_ty.ty, Direction::Incoming, self.arg_ty_span)?;
        Ok(type_info.dotnet_conversion_code.replace(FROM_VAR_TEMPLATE, var_name))
        // if let Some(cpp_conv) = type_info.cpp_converter {
        //     Ok(cpp_conv
        //         .converter
        //         .as_str()
        //         .replace(FROM_VAR_TEMPLATE, var_name))
        // } else {
        //     Ok(var_name.into())
        // }
    }
    fn swig_foreign_from_i_type(&mut self, ty: &syn::Type, var_name: &str) -> Result<String> {
        let rust_ty = self
            .generator
            .conv_map
            .find_or_alloc_rust_type(ty, self.arg_ty_span.0);
        let type_info = map_type(self.generator, &rust_ty.ty, Direction::Outgoing, self.arg_ty_span)?;
        Ok(type_info.dotnet_conversion_code.replace(FROM_VAR_TEMPLATE, var_name))
        // if let Some(cpp_conv) = f_info.cpp_converter {
        //     Ok(cpp_conv
        //         .converter
        //         .as_str()
        //         .replace(FROM_VAR_TEMPLATE, var_name))
        // } else {
        //     Ok(var_name.into())
        // }
    }
}