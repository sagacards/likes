import AviatePrincipal "mo:principal/Principal";
import Blob "mo:base/Blob";
import Debug "mo:base/Debug";
import Ext "mo:ext/Ext";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import List "mo:base/List";
import Nat32 "mo:base/Nat32";
import Option "mo:base/Option";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import TokenIdentifier "mo:encoding/Base32";

shared ({ caller = creator }) actor class Likes () {

    type TokenIndex = Nat;
    type Like = (Principal, TokenIndex, Principal); // Canister, Token, User

    type Stable = [(Principal, [Like])];

    stable var s_likes : Stable = [];

    var likes = HashMap.HashMap<Principal, List.List<Like>>(0, Principal.equal, Principal.hash);

    private func toStable () : Stable {
        Iter.toArray(
            Iter.map<
                (Principal, List.List<Like>),
                (Principal, [Like])
            >(likes.entries(), func (a) {
                (a.0, List.toArray(a.1))
            })
        );
    };

    system func preupgrade () {
        s_likes := toStable();
    };

    system func postupgrade () {
        for ((k, v) in s_likes.vals()) {
            likes.put(k, List.fromArray(v));
        };
        s_likes := [];
    };

    private func put (
        principal   : Principal,
        canister    : Principal,
        index       : TokenIndex,
        user        : Principal,
    ) : () {
        let existing = switch (likes.get(principal)) {
            case (?some) some;
            case _ null;
        };
        let idempotent = Option.isNull(
            List.find<Like>(existing, func (c, i, u) { i == index and c == canister and c == user; })
        );
        if (idempotent) {
            let update = List.push((canister, index, user), existing);
            likes.put(principal, update);
        };
    };

    private func pop (
        principal   : Principal,
        canister    : Principal,
        index       : TokenIndex,
        user        : Principal,
    ) : () {
        let existing = switch (likes.get(principal)) {
            case (?some) some;
            case _ null;
        };
        let update = List.filter<Like>(existing, func (c, i, u) { c != canister and i != index and u != user });
        likes.put(principal, update);
    };

    private func combinePrincipals (
        a : Principal,
        b : Principal,
    ) : Principal {
        var arr : List.List<Nat8> = null;
        var i = 0;
        let bytesA = Blob.toArray(Principal.toBlob(a));
        let bytesB = Blob.toArray(Principal.toBlob(b));
        let size = if (bytesA.size() < bytesB.size()) {
            bytesA.size();
        } else {
            bytesB.size();
        };

        while (i < size and i <= 16) {
            arr := List.push(bytesA[i], arr);
            arr := List.push(bytesB[i], arr);
            i += 1;
        };

        Debug.print(
            Principal.toText(a) # " "
            # Principal.toText(b) # " "
            # Principal.toText(AviatePrincipal.fromBlob(Blob.fromArray(List.toArray(arr))))
        );

        AviatePrincipal.fromBlob(Blob.fromArray(List.toArray(arr)));
    };

    public shared ({ caller }) func like (
        canister    : Principal,
        index       : TokenIndex,
    ) : async () {
        put(combinePrincipals(caller, canister), canister, index, caller);
        put(caller, canister, index, caller);
        put(canister, canister, index, caller);
        put(Principal.fromText(Ext.TokenIdentifier.encode(canister, Nat32.fromNat(index))), canister, index, caller);
    };

    public shared ({ caller }) func unlike (
        canister    : Principal,
        index       : TokenIndex,
    ) : async () {
        pop(combinePrincipals(caller, canister), canister, index, caller);
        pop(caller, canister, index, caller);
        pop(canister, canister, index, caller);
        pop(Principal.fromText(Ext.TokenIdentifier.encode(canister, Nat32.fromNat(index))), canister, index, caller);
    };

    public query ({ caller }) func get (
        canister : ?Principal
    ) : async ?[Like] {
        switch (canister) {
            case (?c) {
                do ? {
                    List.toArray<Like>(likes.get(combinePrincipals(caller, c))!);
                };
            };
            case _ {
                do ? {
                    List.toArray<Like>(likes.get(caller)!);
                };
            };
        };
    };

    public query ({ caller }) func dump () : async Stable {
        assert(caller == creator);
        toStable();
    };

    public shared ({ caller }) func purge () : async () {
        assert(caller == creator);
        likes := HashMap.HashMap<Principal, List.List<Like>>(0, Principal.equal, Principal.hash);
    };

    public query ({ caller }) func count (
        canister    : Principal,
        index       : TokenIndex,
    ) : async Nat {
        switch (do ? { List.size(likes.get(Principal.fromText(Ext.TokenIdentifier.encode(canister, Nat32.fromNat(index))))!); }) {
            case (?c) c;
            case _ 0;
        }
    };

}