use serde::{Serialize, Serializer};
use abra_core::vm::chunk::BindingDescriptor;

pub struct JsBindingDescriptor<'a>(pub &'a BindingDescriptor);

impl<'a> Serialize for JsBindingDescriptor<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer
    {
        use serde::ser::SerializeMap;

        let BindingDescriptor { name, scope_depth } = self.0;

        let mut obj = serializer.serialize_map(Some(2))?;
        obj.serialize_entry("name", name)?;
        obj.serialize_entry("scopeDepth", scope_depth)?;
        obj.end()
    }
}
