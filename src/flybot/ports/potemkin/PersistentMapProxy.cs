using System;
using clojure.lang;
using System.Collections.Generic;
namespace flybot.ports.potemkin
{
    public class PersistentMapProxy : APersistentMap, IObj
	{

        public interface IMap {
            object get(object k, object defaultValue);
            HashSet<object> keySet();
            IMap assoc(object k, object v);
            IMap dissoc(object k);
            IMap empty();
        }

        public interface IEquality {
            bool eq(object o);
            int hash();
        }

        public class MapEntry : clojure.lang.MapEntry
        {
            private new object _key;
            private ILookup _lookup;

            public MapEntry(ILookup lookup, object key) : base(key, null)
            {
                _key = key;
                _lookup = lookup;
            }

            public override object val(){
                return _lookup.valAt(_key, null);
            }

        }

        private readonly IMap _map;
        private readonly IPersistentMap _meta;

		public PersistentMapProxy (IMap map)
		{
            this._map = map;
            this._meta = null;
		}

        public PersistentMapProxy(IMap map, IPersistentMap meta) {
            this._map = map;
            this._meta = meta;
        }

        public IMap innerMap() {
            return _map;
        }

        public IPersistentMap meta(){
            return _meta;
        }

        public override IObj withMeta(IPersistentMap meta){
            return new PersistentMapProxy(_map, meta);
        }

		public override int GetHashCode()
		{
            return (_map is IEquality) ? ((IEquality)_map).hash() : base.GetHashCode();
		}

        public override Boolean Equals (object o) {
            if (_map is IEquality) {
                IEquality map = (IEquality)_map;
                return (o is PersistentMapProxy) ? map.eq(((PersistentMapProxy)o).innerMap()) : map.eq(o);
            }
            return base.Equals(o);
        }

        public override Boolean containsKey(Object key){
            return _map.keySet().Contains(key);
        }

		public override IMapEntry entryAt(object key)
		{
            return containsKey(key) ? new MapEntry(this, key) : null;
		}

        public override IPersistentMap assoc(Object k, Object v) {
            return new PersistentMapProxy(_map.assoc(k, v));
        }

		public override IPersistentMap assocEx(object key, object val)
		{
            if (containsKey(key)) {
                throw new InvalidOperationException("key already contained in map");
            }
            return assoc(key, val);
		}

		public override IPersistentMap without(object key)
		{
            return new PersistentMapProxy(_map.dissoc(key));
		}

		public override object valAt(object key)
		{
            return _map.get(key, null);
		}

		public override object valAt(object key, object notFound)
		{
            return _map.get(key, notFound);
		}

		public override int count()
		{
            return _map.keySet().Count;
		}

		public override IPersistentCollection empty()
		{
            IMap empty = _map.empty();
            return empty != null ? new PersistentMapProxy(_map.empty()) : (IPersistentCollection) PersistentHashMap.EMPTY;
		}

        // not sure if we need more here
        //public IEnumerator<IMapEntry> IEnumerable<IMapEntry>.GetEnumerator(){
        //    foreach(var k in _map.keySet()){
        //        yield return (IMapEntry)MapEntry.create(k, this.valAt(k, null));
        //    }
        //}
        // still need to make it IEnumerable I trow

        IEnumerator<IMapEntry> GetMapEntryEnumerator(){
            foreach(var k in _map.keySet()) {
                yield return clojure.lang.MapEntry.create(k, this.valAt(k, null));
            }
        }

		public override IEnumerator<KeyValuePair<object, object>> GetEnumerator()
		{
			return base.GetEnumerator();
		}

		public override ISeq seq()
        {
            return RT.chunkEnumeratorSeq(this.GetMapEntryEnumerator());
        }

	}
}
