function _statedown(state::String)
    sthash = hash(state) # TODO: make this UInt128 sha1
    sthash = slug(sthash, 8)
    return "/Users/jameson/.julia/compiled/v1.10/stateup_$sthash.ij"
end
function _stateup(state::String)
    return _statedown(state)
end
