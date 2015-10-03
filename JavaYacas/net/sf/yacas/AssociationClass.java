package net.sf.yacas;

import java.util.Comparator;
import java.util.Map;
import java.util.TreeMap;

class AssociationClass extends GenericClass {

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
        this._map = new TreeMap<>(new Cmp(env));
    }

    @Override
    public String Send(LispArgList aArgList) {
        return null;
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

    Map<LispPtr, LispPtr> _map;
}
