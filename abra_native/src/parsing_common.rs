use syn::NestedMeta;
use std::collections::HashMap;

pub fn find_attr<'a>(attrs: &'a Vec<syn::Attribute>, name: &str) -> Option<(usize, &'a syn::Attribute)> {
    attrs.into_iter().enumerate().find(|(_, attr)| {
        attr.path.segments.first().map_or(false, |s| {
            s.ident.to_string() == name.to_string()
        })
    })
}

pub fn parse_attr(attr: &syn::Attribute) -> HashMap<String, String> {
    let mut map = HashMap::new();

    match attr.parse_meta() {
        Err(e) => eprintln!("{:#?}", e),
        Ok(syn::Meta::List(meta)) => {
            for nested in meta.nested {
                if let Some((arg, val)) = parse_attr_meta(nested) {
                    map.insert(arg, val);
                }
            }
        }
        _ => {}
    }

    map
}

pub fn parse_attr_meta(nested: syn::NestedMeta) -> Option<(String, String)> {
    match nested {
        NestedMeta::Meta(syn::Meta::NameValue(nv)) => {
            let arg = nv.path.segments[0].ident.to_string();
            let val = match nv.lit {
                syn::Lit::Str(s) => s.value(),
                syn::Lit::Bool(b) => b.value.to_string(),
                _ => unreachable!()
            };
            Some((arg, val))
        }
        NestedMeta::Meta(syn::Meta::Path(path)) => {
            let bool_arg = path.segments[0].ident.to_string();
            Some((bool_arg, "true".to_string()))
        }
        _ => None
    }
}
