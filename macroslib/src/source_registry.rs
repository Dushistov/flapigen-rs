use crate::SourceCode;

#[derive(Default)]
pub(crate) struct SourceRegistry {
    data: Vec<SourceCode>,
}

impl SourceRegistry {
    pub(crate) fn register(&mut self, src: SourceCode) -> SourceId {
        if self.data.iter().any(|x| x.id_of_code == src.id_of_code) {
            panic!(
                "You try register source code with already existing ID '{}'",
                src.id_of_code
            );
        }
        let id = SourceId(Some(self.data.len()));
        self.data.push(src);
        id
    }

    pub(crate) fn src(&self, src_id: SourceId) -> &str {
        &self.data[src_id.0.expect("Internal Error: Invalid source id")].code
    }

    pub(crate) fn src_with_id(&self, src_id: SourceId) -> &SourceCode {
        &self.data[src_id.0.expect("Internal Error: Invalid source id")]
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct SourceId(Option<usize>);

impl SourceId {
    #[inline]
    pub(crate) const fn none() -> SourceId {
        SourceId(None)
    }
    #[inline]
    pub(crate) fn is_none(&self) -> bool {
        self.0.is_none()
    }
}
