package net.sf.yacas;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

final class AssociationClass extends GenericClass {

    private static class Cmp implements Comparator<LispPtr> {
        public Cmp(LispEnvironment env) {
            this._env = env;
        }
        
        @Override
        public int compare(LispPtr e1, LispPtr e2) {
            try {
                if (LispStandard.InternalStrictTotalOrder(_env, e1, e2))
                    return -1;

                if (LispStandard.InternalEquals(_env, e1, e2))
                    return 0;
                
            } catch (Exception e) {
                // this shouldn't happen, right?
                System.exit(255);
            }
             
            return 1;
            
        }
        
        LispEnvironment _env;
    }
    
    public AssociationClass(LispEnvironment env) {
        this._env = env;
        this._map = new TreeMap<>(new Cmp(env));
    }

    @Override
    public String TypeName() {
        return "\"Association\"";
    }

    public int Size() {
        return _map.size();
    }

    public LispObject GetElement(LispObject k) throws Exception {
        LispPtr v =  _map.get(new LispPtr(k));
        
        if (v != null)
            return v.Get();
        
        return null;
    }

    public void SetElement(LispObject k, LispObject v) throws Exception {
        _map.put(new LispPtr(k), new LispPtr(v));
    }

    public boolean DropElement(LispObject k) throws Exception {
        return _map.remove(new LispPtr(k)) != null;
    }

    public LispPtr Keys() throws Exception {
        LispObject head = LispAtom.New(_env, "List");
        LispPtr p = new LispPtr(head);
        for (Map.Entry<LispPtr, LispPtr> e : _map.entrySet()) {
            p.Get().Set(e.getKey().Get().Copy(true));
            p = p.Get().Next();
        }
        return new LispPtr(LispSubList.New(head));
    }
    
    public LispPtr ToList() throws Exception {
        LispObject head = LispAtom.New(_env, "List");
        LispPtr p = new LispPtr(head);
        for (Map.Entry<LispPtr, LispPtr> e: _map.entrySet()) {
            LispPtr q = new LispPtr(LispAtom.New(_env, "List"));
            p.Get().Set(LispSubList.New(q.Get()));
            p = p.Get().Next();
            q.Get().Set(e.getKey().Get().Copy(true));
            q = q.Get().Next();
            q.Get().Set(e.getValue().Get().Copy(true));
        }
        return new LispPtr(LispSubList.New(head));
    }
    
    LispPtr Head() throws Exception {
        assert(!_map.isEmpty());
        Map.Entry<LispPtr, LispPtr> e = _map.entrySet().iterator().next();
        
        LispObject p = LispAtom.New(_env, "List");
        LispPtr q = new LispPtr(p);
        q.Get().Set(e.getKey().Get().Copy(true));
        q = q.Get().Next();
        q.Get().Set(e.getValue().Get().Copy(true));
        
        return new LispPtr(LispSubList.New(p));
    }
    
    LispEnvironment _env;
    Map<LispPtr, LispPtr> _map;
}
