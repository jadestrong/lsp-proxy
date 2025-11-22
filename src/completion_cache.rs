use crate::lsp_ext::CompletionItem;
use log::debug;

pub struct CompletionCache {
    pub uri: Option<String>,
    pub bounds_start: Option<i32>,
    pub pretext: Option<String>,
    pub prefix: Option<String>,
    pub items: Option<Vec<CompletionItem>>,
}

impl CompletionCache {
    pub fn new() -> Self {
        Self {
            uri: None,
            bounds_start: None,
            pretext: None,
            prefix: None,
            items: None,
        }
    }

    pub fn clear_cache(&mut self) {
        self.uri = None;
        self.bounds_start = None;
        self.pretext = None;
        self.items = None;
        self.prefix = None;
    }

    pub fn set_cache(
        &mut self,
        uri: Option<String>,
        bounds_start: i32,
        pretext: String,
        prefix: String,
        items: Vec<CompletionItem>,
    ) {
        self.uri = uri;
        self.bounds_start = Some(bounds_start);
        self.pretext = Some(pretext);
        self.items = Some(items);
        self.prefix = Some(prefix);
    }

    pub fn get_cached_items(
        &self,
        new_uri: &String,
        new_pretext: &str,
        new_prefix: &str,
        new_bounds_start: &i32,
    ) -> Option<Vec<CompletionItem>> {
        match self {
            CompletionCache {
                uri: Some(uri),
                items: Some(items),
                pretext: Some(pretext),
                prefix: Some(prefix),
                bounds_start: Some(bounds_start),
            } => {
                if prefix.is_empty() && !new_prefix.is_empty() {
                    debug!("ignore cache1");
                    return None;
                }
                if prefix.is_empty()
                    && new_prefix.is_empty()
                    && uri == new_uri
                    && new_bounds_start == bounds_start
                {
                    debug!("reuse empty prefix cache");
                    return Some(items.to_owned());
                }
                if !prefix.is_empty() && new_prefix.is_empty() {
                    debug!("ignore cache2");
                    return None;
                }
                debug!("new_pretext {new_pretext} ~~~ {pretext}");
                if uri == new_uri
                    && new_pretext.starts_with(pretext)
                    && new_bounds_start == bounds_start
                {
                    return Some(items.to_owned());
                }
                None
            }
            _ => None,
        }
    }
}
