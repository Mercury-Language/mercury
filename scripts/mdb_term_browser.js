// Copyright (C) 2017-2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

const OPEN_DEPTH = 3;
const OPEN_MAX_ARGS = 10;

function escapeHTML(html) {
    return document.createElement('div')
        .appendChild(document.createTextNode(html))
        .parentNode.innerHTML;
}

function term_functor_to_html(term)
{
    return escapeHTML(term.functor);
}

function term_oneline_to_html(term)
{
    if (term.oneline) {
        return escapeHTML(term.oneline);
    } else {
        return term_functor_to_html(term);
    }
}

function term_to_json(term)
{
    const field = {
        name: "term",
        term: term
    };
    return field_to_json(field, 0, true);
}

function field_to_json(field, open_depth, root)
{
    const name_html = field_name_to_html(field.name);
    const term = field.term;

    var initial_text;
    var initial_open;
    var children;
    var userdata = {}
    if (term.args) {
        userdata.opened_text = name_html + term_functor_to_html(term);
        userdata.closed_text = name_html + term_oneline_to_html(term);
        initial_open = false;
        if (term.oneline_elided) {
            if (root) {
                initial_open = true;
            } else {
                initial_open = (open_depth < OPEN_DEPTH &&
                    term.args.length < OPEN_MAX_ARGS);
            }
        }
        if (initial_open) {
            initial_text = userdata.opened_text;
        } else {
            initial_text = userdata.closed_text;
        }
        var new_depth;
        if (initial_open) {
            new_depth = open_depth + 1;
        } else {
            // Reset depth so that opening a closed node reveals
            // some of its children as well.
            new_depth = 0;
        }
        children = term.args.map(function(arg) {
            return field_to_json(arg, new_depth, false);
        });
    } else {
        initial_text = name_html + term_oneline_to_html(term);
        initial_open = false;
        children = false;
    }

    if (typeof(field.name) == "string") {
        userdata.search_name = field.name;
    }
    userdata.search_value = term.functor.toString();

    return {
        text: initial_text,
        state: {opened: initial_open},
        children: children,
        a_attr: {title: term.type},
        data: userdata
    };
}

function field_name_to_html(name)
{
    if (typeof(name) == "number") {
        return '<span class="pos">[' + name + ']</span>';
    } else {
        return '<span class="name">' + escapeHTML(name) + '</span>';
    }
}

function short_search_callback(s, node)
{
    const userdata = node.data;
    if (userdata.search_name && userdata.search_name.startsWith(s)) {
        return true;
    }
    return userdata.search_value.startsWith(s);
}

function long_search_callback(s, node)
{
    const userdata = node.data;
    if (userdata.search_name && userdata.search_name.indexOf(s) !== -1) {
        return true;
    }
    return userdata.search_value.indexOf(s) !== -1;
}

function choose_search_callback(s)
{
    if (s.length < 3) {
        return short_search_callback;
    } else {
        return long_search_callback;
    }
}

function setup(term)
{
    const treeview = $('#treeview');

    var term_stack = [term_to_json(term)];

    // Create jstree instance.
    treeview.jstree({
        core: {
            data: [term_stack[0]],
            check_callback: true,   // enable modifications (including rename)
            multiple: false,        // no multiple selection
            animation: 0,           // no animation
            themes: {icons: false}  // no icons
        },
        plugins: ["search", "contextmenu"],
        search: {
            search_callback: short_search_callback
        },
        contextmenu: {
            select_node: false,
            items: {
                expand_all: {
                    label: "Expand all",
                    action: expand_all_action
                },
                collapse_all: {
                    label: "Collapse all",
                    action: collapse_all_action
                },
                view_subterm: {
                    label: "View subterm",
                    action: view_subterm_action
                },
                back: {
                    label: "Back to previous term",
                    action: back_action
                }
            }
        }
    });

    // Keep reference to jstree instance.
    const inst = treeview.jstree(true);

    function expand_all_action(data) {
        const obj = inst.get_node(data.reference);
        inst.open_all(obj);
    }
    function collapse_all_action(data) {
        const obj = inst.get_node(data.reference);
        inst.close_all(obj);
    }
    function view_subterm_action(data) {
        const obj = inst.get_node(data.reference);
        // This seems to work...
        inst.move_node(obj, treeview, 0);
        inst.delete_node(inst.get_next_dom(obj, true));

        term_stack.push(inst.get_json(obj));
    }
    function back_action(data) {
        if (term_stack.length > 1) {
            term_stack.pop();
            inst.settings.core.data = term_stack[term_stack.length - 1];
            inst.refresh();
        }
    }

    function on_open_node(e, data) {
        const node = data.node;
        const userdata = node.data;
        inst.rename_node(node, userdata.opened_text);
    }
    function on_close_node(e, data) {
        const node = data.node;
        const userdata = node.data;
        inst.rename_node(node, userdata.closed_text);
    }
    function on_select_node(e, data) {
        const node = data.node;
        inst.toggle_node(node);
    }
    treeview.on('open_node.jstree', on_open_node);
    treeview.on('close_node.jstree', on_close_node);
    treeview.on('select_node.jstree', on_select_node);

    const searchbox = $('#searchbox');
    var timeout = false;
    searchbox.keyup(function() {
        if (timeout) {
            clearTimeout(timeout);
        }
        timeout = setTimeout(
            function() {
                const v = searchbox.val();
                inst.settings.search.search_callback =
                    choose_search_callback(v);
                inst.search(v);
            },
            250
        );
    });
}

$(document).ready(function() {
    setup(term);
    term = null;    // don't need it any more
});
