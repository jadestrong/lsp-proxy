//! Small stdio peer for protocol tests; exercises the real Client/Transport.
use crate::{client::Client, lsp::jsonrpc::Call};
use std::{collections::HashMap, sync::Arc};
use tokio::sync::mpsc::UnboundedReceiver;

const PEER: &str = r#"
import sys,json
name,uri=sys.argv[1:3]
replies=[]
pending={}
definition_calls=0
initialize_params=None
configurations=[]
watched_files=[]
diagnostic_calls=[]
held_diagnostics=[]
held_requests=[]
def send(value):
    value['jsonrpc']='2.0'
    body=json.dumps(value).encode()
    sys.stdout.buffer.write(('Content-Length: %d\r\n\r\n'%len(body)).encode()+body)
    sys.stdout.buffer.flush()
def raw(body):
    sys.stdout.buffer.write(('Content-Length: %d\r\n\r\n'%len(body)).encode()+body)
    sys.stdout.buffer.flush()
def answer(mid,value): send({'id':mid,'result':value})
def diagnostic_answer(mid,p):
    if name=='diagnostic-error':
        send({'id':mid,'error':{'code':-32603,'message':'diagnostic failed'}})
        return
    if name=='diagnostic-unchanged':
        answer(mid,{'kind':'unchanged','resultId':'unchanged-result'})
        return
    items=[] if p['textDocument']['uri'].endswith('Clean.ts') else [{'range':{'start':{'line':0,'character':0},'end':{'line':0,'character':1}},'message':name,'severity':2}]
    report={'kind':'full','resultId':'result-'+str(mid),'items':items}
    if name=='diagnostic-related':
        report['relatedDocuments']={'file:///other/Other.ts':{'kind':'full','resultId':'external','items':items}}
    answer(mid,report)
while True:
    headers={}
    while True:
        line=sys.stdin.buffer.readline()
        if not line: sys.exit(0)
        if line==b'\r\n': break
        k,v=line.decode().split(':',1);headers[k.lower()]=v.strip()
    msg=json.loads(sys.stdin.buffer.read(int(headers['content-length'])))
    method=msg.get('method'); mid=msg.get('id'); p=msg.get('params',{})
    if method is None:
        replies.append(msg)
        if mid in pending:
            request=pending.pop(mid)
            if 'error' in msg: send({'id':request,'error':msg['error']})
            else: answer(request,msg['result'])
    elif method=='initialize':
        initialize_params=p
        provider={} if name=='hierarchy-options' else name!='hierarchy-unsupported'
        answer(mid,{'capabilities':{'callHierarchyProvider':provider,'typeHierarchyProvider':provider}})
    elif method=='test/initializeParams': answer(mid,initialize_params)
    elif method=='test/hold': held_requests.append(mid)
    elif method=='test/heldRequests': answer(mid,len(held_requests))
    elif method=='test/missingResult': send({'id':mid})
    elif method=='test/malformedResponse': send({'id':mid,'error':{'code':'bad','message':'malformed'}})
    elif method=='test/bothResults': send({'id':mid,'result':None,'error':{'code':-32042,'message':'failure'}})
    elif method=='test/rpcError': send({'id':mid,'error':{'code':-32042,'message':'real RPC failure','data':{'detail':42}}})
    elif method=='test/malformedMessages':
        raw(b'{broken JSON')
        raw(b'"\xff"')
        raw(b'[]')
        raw(b'{"jsonrpc":"2.0","method":3}')
        answer(mid,'still alive')
    elif method in ('test/closeOutput','test/truncatedFrame','test/closeInput','test/exitNotification'):
        import os,time
        if method=='test/closeInput':
            os.close(0)
            answer(mid,'input closed')
        elif method=='test/exitNotification': send({'method':'exit'})
        else:
            if method=='test/truncatedFrame':
                sys.stdout.buffer.write(b'Content-Length: 100\r\n\r\n{}')
                sys.stdout.buffer.flush()
            os.close(1)
        time.sleep(60)
    elif method in ('textDocument/prepareCallHierarchy','textDocument/prepareTypeHierarchy') or method.startswith(('callHierarchy/','typeHierarchy/')):
        if name=='hierarchy-error':
            send({'id':mid,'error':{'code':-32042,'message':'hierarchy RPC failure'}})
            continue
        if name=='hierarchy-invalid':
            answer(mid,{'unexpected':True})
            continue
        if name=='hierarchy-null':
            answer(mid,None)
            continue
        rng={'start':{'line':0,'character':1},'end':{'line':0,'character':4}}
        item={'name':'Bean','kind':5,'uri':uri,'range':rng,'selectionRange':rng,'data':{'owner':name,'opaque':[1,{'x':'y'}]}}
        if name in ('hierarchy-tags-empty','hierarchy-tags-deprecated'):
            item['tags']=[] if name.endswith('empty') else [1]
        if method.startswith('textDocument/'):
            if name=='hierarchy-virtual': assert p['position']['line']==1
            answer(mid,[item])
        else:
            assert p['item']['data']['owner']==name
            assert p['item']['data']['opaque']==[1,{'x':'y'}]
            if 'tags' in item: assert p['item']['tags']==item['tags']
            if method.startswith('callHierarchy/'):
                key='from' if method.endswith('incomingCalls') else 'to'
                answer(mid,[{key:item,'fromRanges':[rng]}])
            else: answer(mid,[item])
    elif method=='workspace/symbol':
        if name=='bad': send({'id':mid,'error':{'code':-32603,'message':'search failed'}})
        elif name=='null': answer(mid,None)
        else:
            symbol={'name':p['query'],'kind':5,'location':{'uri':uri},'data':{'owner':name}}
            if name!='new': symbol['location']['range']={'start':{'line':0,'character':1},'end':{'line':0,'character':4}}
            answer(mid,[symbol])
    elif method=='workspaceSymbol/resolve':
        assert p['data']['owner']==name
        p['location']['range']={'start':{'line':2,'character':1},'end':{'line':2,'character':4}}
        answer(mid,p)
    elif method in ('textDocument/definition','textDocument/declaration','textDocument/typeDefinition','textDocument/implementation'):
        definition_calls+=1
        location={'uri':uri,'range':{'start':{'line':2,'character':1},'end':{'line':2,'character':4}}}
        if name=='goto-null' or (name=='goto-transient' and definition_calls==1): answer(mid,None)
        elif name=='goto-empty': answer(mid,[])
        elif name=='goto-scalar': answer(mid,location)
        elif name=='goto-links': answer(mid,[{'targetUri':uri,'targetRange':{'start':{'line':0,'character':0},'end':{'line':4,'character':0}},'targetSelectionRange':location['range']}])
        elif name=='goto-invalid': answer(mid,{'uri':uri})
        elif name=='goto-error': send({'id':mid,'error':{'code':-32042,'message':'goto RPC failure'}})
        else: answer(mid,[location])
    elif method=='workspace/didChangeConfiguration': configurations.append(p['settings'])
    elif method=='test/configurations': answer(mid,configurations)
    elif method=='workspace/didChangeWatchedFiles': watched_files.extend(p['changes'])
    elif method=='test/watchedFiles': answer(mid,watched_files)
    elif method=='textDocument/diagnostic':
        diagnostic_calls.append({'id':mid,'params':p})
        if name=='diagnostic-held': held_diagnostics.append((mid,p))
        else: diagnostic_answer(mid,p)
    elif method=='test/diagnosticCalls': answer(mid,diagnostic_calls)
    elif method=='test/releaseDiagnostics':
        for request,params in held_diagnostics: diagnostic_answer(request,params)
        held_diagnostics=[]
        answer(mid,None)
    elif method=='test/replies': answer(mid,replies)
    elif method=='workspace/executeCommand':
        command=p['command']
        if command=='sts.java.addClasspathListener':
            assert len(p['arguments'])==2 and isinstance(p['arguments'][1],bool)
            callback=p['arguments'][0]
            cid='callback-'+str(mid)
            pending[cid]=mid
            send({'id':cid,'method':'workspace/executeClientCommand','params':{'command':callback,'arguments':[[uri,'project',False,{'entries':[]},None]]}})
        elif command=='sts.java.removeClasspathListener': answer(mid,'removed')
        elif command=='error': send({'id':mid,'error':{'code':-32042,'message':'callback failed','data':{'owner':name}}})
        else: answer(mid,{'server':name,'arguments':p.get('arguments',[])})
    elif method=='exit': sys.exit(0)
    elif mid is not None: answer(mid,None)
"#;

pub(crate) async fn client(
    name: &str,
    id: usize,
    uri: &str,
    initialized: bool,
) -> (Arc<Client>, UnboundedReceiver<(usize, Call)>) {
    let (client, mut rx, notify, _) = Client::start(
        "python3",
        &[
            "-u".into(),
            "-c".into(),
            PEER.into(),
            name.into(),
            uri.into(),
        ],
        None,
        None,
        HashMap::new(),
        &[],
        id,
        name.into(),
        2,
        None,
        None,
    )
    .unwrap();
    let client = Arc::new(client);
    if initialized {
        initialize(&client);
    }
    notify.notify_one();
    // Consume the synthetic startup notification while the test receiver is
    // still alive. Tests may retain only Client to exercise request/response IO.
    let (_, startup) = tokio::time::timeout(std::time::Duration::from_secs(2), rx.recv())
        .await
        .unwrap()
        .unwrap();
    assert!(matches!(startup, Call::Notification(n) if n.method == "initialized"));
    (client, rx)
}

pub(crate) fn initialize(client: &Client) {
    client
        .capabilities
        .set(lsp_types::ServerCapabilities {
            workspace_symbol_provider: Some(lsp_types::OneOf::Left(true)),
            ..Default::default()
        })
        .unwrap();
}

pub(crate) fn request(method: &str, context: Option<crate::msg::Context>) -> crate::msg::Request {
    crate::msg::Request {
        id: 42.into(),
        method: method.into(),
        params: crate::msg::Params {
            uri: None,
            context,
            virtual_doc: None,
            params: serde_json::Value::Null,
        },
    }
}
